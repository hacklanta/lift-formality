package com.hacklanta
package formality

import scala.language.experimental.macros

import net.liftweb.common._
  import HListies._
import net.liftweb.util._
  import Helpers._

object FieldGroup {
  import scala.reflect.macros.whitebox.Context

  def withFieldsImpl(c: Context)(fields: c.Expr[FieldHolderBase[_]]*) = {
    FormalityFormProto.buildFieldsExpression(c)(fields)
  }

  def formalizeImpl(c: Context) = {
    import c.universe._

    val (boxedHlistType, boxedHlistTypes, hlistType, hlistTypes) =
      c.prefix.actualType match {
        case TypeRef(_, _, _ :: boxedValueTypes :: headValueType :: restValueTypes :: Nil) =>
          (
            tq"$boxedValueTypes",
            FormalityFormHelper.unwindHlistTypes(c)(boxedValueTypes),
            tq"net.liftweb.common.HLists.HCons[$headValueType, $restValueTypes]",
            headValueType :: FormalityFormHelper.unwindHlistTypes(c)(restValueTypes)
          )

        case TypeRef(_, thing, other) =>
          (tq"net.liftweb.common.HLists.HNil", Nil, tq"net.liftweb.common.HLists.HNil", Nil)
      }

    if (hlistTypes.length > 22) {
      q"""{
        val initialGroup = ${c.prefix}

        new FieldGroup(
          initialGroup.scopingSelector,
          initialGroup.converter,
          initialGroup.computeValues,
          initialGroup.bindFields
        )
      }"""
    } else {
      val (matcher, parameterList) =
        (0 until hlistTypes.length)
          .reverse
          .foldLeft((q"HNil": c.Tree, List[Ident]())) {
            case ((typeMatch, parameters), _) =>
              val name = TermName(c.freshName("boxedFieldValue$"))

              (pq""":+:($name, $typeMatch)""", q"$name" :: parameters)
          }

      q"""{
        val initialGroup = ${c.prefix}

        new FieldGroup(
          initialGroup.scopingSelector,
          initialGroup.converter,
          initialGroup.computeValues,
          initialGroup.bindFields
        ) {
          def withConverterFn[T](converter: (..$hlistTypes)=>net.liftweb.common.Box[T]) = {
            withHlistConverter[T]({
              case $matcher =>
                converter.apply(..$parameterList)
            })
          }

          def withBoxedConverterFn[T](converter: (..$boxedHlistTypes)=>net.liftweb.common.Box[T]) = {
            withBoxedHlistConverter[T]({
              case $matcher =>
                converter.apply(..$parameterList)
            })
          }

          def asFn[T](converter: (..$hlistTypes)=>T) = {
            withHlistConverter[T]({
              case $matcher =>
                net.liftweb.common.Full(converter.apply(..$parameterList))
            })
          }
        }
      }"""
    }
  }

  def withConverterImpl[T](c: Context)(newConverter: c.Expr[T]) = {
    import c.universe._

    q"${formalizeImpl(c)}.withConverterFn($newConverter)"
  }

  def withBoxedConverterImpl[T](c: Context)(newConverter: c.Expr[T]) = {
    import c.universe._

    q"${formalizeImpl(c)}.withBoxedConverterFn($newConverter)"
  }

  def asImpl[T](c: Context)(newConverter: c.Expr[T]) = {
    import c.universe._

    q"${formalizeImpl(c)}.asFn($newConverter)"
  }

  def apply() = {
    FieldGroupBase(None)
  }
  def apply(scopingSelector: String) = {
    FieldGroupBase(Some(scopingSelector))
  }
}
case class FieldGroupBase private[formality](scopingSelector: Option[String]) {
  def withField[FieldValueType](field: FieldHolderBase[FieldValueType]) = {
    FieldGroup[
      FieldValueType :+: HNil,
      Box[FieldValueType] :+: HNil,
      FieldValueType,
      HNil
    ](
      scopingSelector,
      converter = Left(Full.apply _),
      computeValues = () => {
        (field.value :+: HNil, field.value: CombinableBoxie.CombinableBox[FieldValueType, HNil])
      },
      bindFields = field.binder
    )
  }

  def withFields(fields: FieldHolderBase[_]*): FieldGroup[_,_,_,_] = macro FieldGroup.withFieldsImpl
}
case class FieldGroup[
  CombinedType,
  FieldBoxList <: HList,
  HeadFieldValueType,
  RestFieldValueTypes <: HList
](
  scopingSelector: Option[String],
  converter: Either[(HeadFieldValueType :+: RestFieldValueTypes)=>Box[CombinedType],(FieldBoxList)=>Box[CombinedType]],
  computeValues: ()=>(FieldBoxList, CombinableBoxie.CombinableBox[HeadFieldValueType, RestFieldValueTypes]),
  bindFields: CssSel
) extends FieldHolderBase[CombinedType] {
  def withHlistConverter[T](newConverter: (HeadFieldValueType :+: RestFieldValueTypes)=>Box[T]): FieldGroup[T, FieldBoxList, HeadFieldValueType, RestFieldValueTypes] = {
    copy[T, FieldBoxList, HeadFieldValueType, RestFieldValueTypes](
      converter = Left(newConverter)
    )
  }
  def withBoxedHlistConverter[T](newConverter: (FieldBoxList)=>Box[T]): FieldGroup[T, FieldBoxList, HeadFieldValueType, RestFieldValueTypes] = {
    copy[T, FieldBoxList, HeadFieldValueType, RestFieldValueTypes](
      converter = Right(newConverter)
    )
  }
  def withConverter[T](newConverter: T): FieldGroup[_,_,_,_] = macro FieldGroup.withConverterImpl[T]
  def withBoxedConverter[T](newConverter: T): FieldGroup[_,_,_,_] = macro FieldGroup.withBoxedConverterImpl[T]
  def as[T](newConverter: T): FieldGroup[_,_,_,_] = macro FieldGroup.asImpl[T]

  def formalize: FieldGroup[_,_,_,_] = macro FieldGroup.formalizeImpl

  def withField[FieldValueType](field: FieldHolderBase[FieldValueType]) = {
    this.copy[
      FieldValueType :+: HeadFieldValueType :+: RestFieldValueTypes,
      Box[FieldValueType] :+: FieldBoxList,
      FieldValueType,
      HeadFieldValueType :+: RestFieldValueTypes
    ](
      converter = Left(Full.apply _),
      computeValues = () => {
        val (boxes, combinedResult) = this.computeValues()
        (field.value :+: boxes, field.value :&: combinedResult)
      },
      bindFields = this.bindFields & field.binder
    )
  }
  def withFields(fields: FieldHolderBase[_]*): FieldGroup[_, _, _, _] = macro FieldGroup.withFieldsImpl

  val fieldValue = new FieldValueVar[CombinedType]({
    val (boxedValueResult, valueResult) = computeValues()

    converter match {
      case Left(unboxedConverter) =>
        valueResult.rhs match {
          case Right(values) =>
            unboxedConverter(values)
          case Left(failures) =>
            ParamFailure("Failed to build combined group.", Empty, Empty, CombinableBoxie.FailureList(failures))
        }

      case Right(boxedConverter) =>
        boxedConverter(boxedValueResult)
    }
  })
  def value = fieldValue.is

  def binder: CssSel = {
    scopingSelector.map { selector =>
      selector #> bindFields
    } getOrElse {
      bindFields
    }
  }
}
