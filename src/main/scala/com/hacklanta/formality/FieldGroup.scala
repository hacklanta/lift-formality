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

    val (hlistType, hlistTypes) =
      c.prefix.actualType match {
        case TypeRef(_, _, _ :: _ :: headValueType :: restValueTypes :: Nil) =>
          (
            tq"net.liftweb.common.HLists.HCons[$headValueType, $restValueTypes]",
            headValueType :: FormalityFormHelper.unwindHlistTypes(c)(restValueTypes)
          )

        case TypeRef(_, thing, other) =>
          (tq"net.liftweb.common.HLists.HNil", Nil)
      }

    if (hlistTypes.length > 22) {
      q"""{
        val initialGroup = ${c.prefix}

        new FieldGroup(
          initialGroup.scopingSelector,
          initialGroup.converter,
          initialGroup.computeValues,
          initialGroup.bindFields
        ) {
          def withConverterFn[T](converter: ($hlistType)=>net.liftweb.common.Box[T]) = {
            withHlistConverter[T](converter)
          }
        }
      }"""
    } else {
      val (matcher, parameterList) =
        (0 until hlistTypes.length)
          .reverse
          .foldLeft((q"HNil": c.Tree, List[Ident]())) {
            case ((typeMatch, parameters), _) =>
              val name = TermName(c.freshName("fieldValue$"))

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
        }
      }"""
    }
  }

  def withConverterImpl[T](c: Context)(newConverter: c.Expr[T]) = {
    import c.universe._

    q"${formalizeImpl(c)}.withConverterFn($newConverter)"
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
      converter = Full.apply _,
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
  converter: (HeadFieldValueType :+: RestFieldValueTypes)=>Box[CombinedType],
  computeValues: ()=>(FieldBoxList, CombinableBoxie.CombinableBox[HeadFieldValueType, RestFieldValueTypes]),
  bindFields: CssSel
) extends FieldHolderBase[CombinedType] {
  def withHlistConverter[T](newConverter: (HeadFieldValueType :+: RestFieldValueTypes)=>Box[T]): FieldGroup[T, FieldBoxList, HeadFieldValueType, RestFieldValueTypes] = {
    copy[T, FieldBoxList, HeadFieldValueType, RestFieldValueTypes](
      converter = newConverter
    )
  }
  def withConverter[T](newConverter: T): FieldGroup[_,_,_,_] = macro FieldGroup.withConverterImpl[T]

  def formalize: FieldGroup[_,_,_,_] = macro FieldGroup.formalizeImpl

  def withField[FieldValueType](field: FieldHolderBase[FieldValueType]) = {
    this.copy[
      FieldValueType :+: HeadFieldValueType :+: RestFieldValueTypes,
      Box[FieldValueType] :+: FieldBoxList,
      FieldValueType,
      HeadFieldValueType :+: RestFieldValueTypes
    ](
      converter = Full.apply _,
      computeValues = () => {
        val (boxes, combinedResult) = this.computeValues()
        (field.value :+: boxes, field.value :&: combinedResult)
      },
      bindFields = this.bindFields & field.binder
    )
  }
  def withFields(fields: FieldHolderBase[_]*): FieldGroup[_, _, _, _] = macro FieldGroup.withFieldsImpl

  val fieldValue = new FieldValueVar[CombinedType]({
    val (_, valueResult) = computeValues()

    valueResult.rhs match {
      case Right(values) =>
        converter(values)
      case Left(failures) =>
        ParamFailure("Failed to build combined group.", Empty, Empty, CombinableBoxie.FailureList(failures))
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
