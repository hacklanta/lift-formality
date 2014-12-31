package com.hacklanta
package formality

import scala.language.experimental.macros

import net.liftweb.common.{Box, Failure}
import net.liftweb.http.SHtml
import net.liftweb.http.js.JsCmd
import net.liftweb.http.js.JsCmds._
import net.liftweb.util._
  import Helpers._

import net.liftweb.common.HListies._
import net.liftweb.common.CombinableBoxie

object FormalityFormProto {
  import scala.reflect.macros.whitebox.Context

  // Builds the expression for accumulating fields onto the form. Note that this
  // expression accumulates the fields backwards; because the fields are kept in
  // an HList, the last field added is the first field we deal with later, so
  // the fields are actually being added in the "right order".
  private def buildFieldsExpression(c: Context)(fields: Seq[c.Expr[FieldHolderBase[_]]]) = {
    import c.universe._

    fields.reverse.foldLeft(q"""${c.prefix}""") { (expression, field) =>
      q"""${expression}.withField($field)"""
    }
  }

  private def withFieldsHelper(c: Context)(fields: Seq[c.Expr[FieldHolderBase[_]]], formalizeCall: c.universe.TermName) = {
    import c.universe._

    // Split out into two expressions as otherwise the resulting expression gets
    // the compiler confused on types.
    q"""{
      val formWithFields = ${buildFieldsExpression(c)(fields)}
      formWithFields.${formalizeCall}
    }"""
  }

  def withFieldsImpl(c: Context)(fields: c.Expr[FieldHolderBase[_]]*) = {
    withFieldsHelper(c)(fields, c.universe.TermName("formalize"))
  }

  def withAjaxFieldsImpl(c: Context)(fields: c.Expr[FieldHolderBase[_]]*) = {
    withFieldsHelper(c)(fields, c.universe.TermName("ajaxFormalize"))
  }

  def apply() = {
    FormalityFormBase()
  }
}
case class FormalityFormBase private[formality]() {
  /**
   * Adds the specified field to the '''front''' of this form.
   *
   * This means that calling `myForm.withField(field1).withField(field2)` will
   * give you those fields in reverse order in your submission handlers
   * (`field2`'s value first, then `field1`'s). It's recommended that you
   * instead use the `withFields` or `withAjaxFields` helpers, which let you add
   * a group of fields all at once in proper order and leaves you with a form
   * that can accept submission handlers.
   * 
   * @return A new `FormailtyFormProto` with the given field prepended to its
   *         list of fields. Note that you still need to call `formalize` or
   *         `ajaxFormalize` on the form if you want to attach submission
   *         handlers.
   */
  def withField[
    IncomingValueType,
    FieldValueType,
    ValidationType >: FieldValueType,
    EventHandlerType >: FieldValueType
  ](
    field: BaseFieldHolder[IncomingValueType, FieldValueType, ValidationType, EventHandlerType]
  ) = {
    FormalityFormProto[
      FieldHolderBase[FieldValueType] :+: HNil,
      Box[FieldValueType] :+: HNil,
      FieldValueType,
      HNil
    ](
      fields = field :+: HNil,
      computeValues = () => {
        (field.value :+: HNil, field.value: CombinableBoxie.CombinableBox[FieldValueType, HNil])
      },
      bindFields = field.binder
    )
  }

  def withFields(fields: FieldHolderBase[_]*): FormalityForm[_, _, _, _, _, _] = macro FormalityFormProto.withFieldsImpl
  def withAjaxFields(fields: FieldHolderBase[_]*): FormalityForm[_, _, _, _, _, _] = macro FormalityFormProto.withAjaxFieldsImpl
}
/**
 * FormalityFormProto is the starting point for creating a formality
 * form. It uses a lot of type magic, so that is going to be detailed
 * here. At its most basic, however, it simply tracks a type-preserving
 * list of FieldHolderBases and provides a single function, withField,
 * to add a new such field to the form.
 *
 * Once you've added all the fields you want, call formalize. This returns
 * a FormalityForm, on which you can set success/failure/submission
 * handlers, and which provides the facility to convert the form down
 * to a single CssSel instance that will properly set up the form and all
 * of its fields.
 *
 * Typing discussion
 * ---------------
 * 
 * shapeless lets us track our list of fields that can be String
 * fields, Int fields, DateMidnight fields, or any other arbitrary
 * type, while preserving those types and their order. As we build
 * this list up, we call withField and add fields to the list one by
 * one.
 *
 * Because of what we need to do once we convert to a FormalityForm.
 */
case class FormalityFormProto[
  FieldList <: HList,
  FieldBoxList <: HList,
  HeadFieldValueType,
  RestFieldValueTypes <: HList
] private[formality] (
  fields: FieldList,
  computeValues: ()=>(FieldBoxList, CombinableBoxie.CombinableBox[HeadFieldValueType, RestFieldValueTypes]),
  bindFields: CssSel
) {
  /**
   * Adds the specified field to the '''front''' of this form.
   *
   * This means that calling `myForm.withField(field1).withField(field2)` will
   * give you those fields in reverse order in your submission handlers
   * (`field2`'s value first, then `field1`'s). It's recommended that you
   * instead use the `withFields` or `withAjaxFields` helpers, which let you add
   a group of fields * all at once in proper order and leaves you with a form
   * that can accept submission handlers.
   * 
   * @return A new `FormailtyFormProto` with the given field prepended to its
   *         list of fields. Note that you still need to call `formalize` or
   *         `ajaxFormalize` on that form if you want to attach submission
   *         handlers.
   */
  def withField[
    IncomingValueType,
    FieldValueType,
    ValidationType >: FieldValueType,
    EventHandlerType >: FieldValueType
  ](
    field: BaseFieldHolder[IncomingValueType, FieldValueType, ValidationType, EventHandlerType]
  ) = {
    this.copy[
      FieldHolderBase[FieldValueType] :+: FieldList,
      Box[FieldValueType] :+: FieldBoxList,
      FieldValueType,
      HeadFieldValueType :+: RestFieldValueTypes
    ](
      fields = field :+: fields,
      computeValues = () => {
        val (boxes, combinedResult) = this.computeValues()
        (field.value :+: boxes, field.value :&: combinedResult)
      },
      bindFields = this.bindFields & field.binder
    )
  }

  def withFields(fields: FieldHolderBase[_]*): FormalityForm[_, _, _, _, _, _] = macro FormalityFormProto.withFieldsImpl
  def withAjaxFields(fields: FieldHolderBase[_]*): FormalityForm[_, _, _, _, _, _] = macro FormalityFormProto.withAjaxFieldsImpl

  def formalize: StandardFormalityForm[FieldList, FieldBoxList, HeadFieldValueType :+: RestFieldValueTypes, _, _] = macro StandardFormalityForm.buildForm[FieldList, FieldBoxList, HeadFieldValueType, RestFieldValueTypes]
  def ajaxFormalize: AjaxFormalityForm[FieldList, FieldBoxList, HeadFieldValueType :+: RestFieldValueTypes, _, _] = macro AjaxFormalityForm.buildForm[FieldList, FieldBoxList, HeadFieldValueType, RestFieldValueTypes]
}

abstract class FormalityForm[
  FieldList <: HList,
  FieldBoxList <: HList,
  FieldValueList <: HList,
  RequestResultType,
  FunctionType,
  BoxedFunctionType
] {
  def onSubmission(handler: BoxedFunctionType): StandardFormalityForm[FieldList, FieldBoxList, FieldValueList, FunctionType, BoxedFunctionType] = macro FormalityFormHelper.onSubmissionImpl[BoxedFunctionType, FieldBoxList, RequestResultType]
  def onSubmissionHList(handler: (FieldBoxList)=>Unit): FormalityForm[FieldList,FieldBoxList,FieldValueList,RequestResultType, FunctionType, BoxedFunctionType]

  def onSuccess(handler: FunctionType): FormalityForm[FieldList,FieldBoxList,FieldValueList,RequestResultType, FunctionType, BoxedFunctionType] = macro FormalityFormHelper.onSuccessImpl[FunctionType, FieldValueList, RequestResultType]
  def onSuccessHList(handler: (FieldValueList)=>RequestResultType): FormalityForm[FieldList,FieldBoxList,FieldValueList,RequestResultType, FunctionType, BoxedFunctionType]

  def onFailure(handler: (List[Failure])=>RequestResultType): FormalityForm[FieldList, FieldBoxList, FieldValueList, RequestResultType, FunctionType, BoxedFunctionType]

  def fields: FieldList
  def bindFields: CssSel
  def submitBind: CssSel

  def binder = {
    bindFields &
    submitBind
  }
}

object FormalityFormHelper {
  import scala.reflect.macros.whitebox.Context

  def onSuccessImpl[T: c.WeakTypeTag, FieldList: c.WeakTypeTag, ReturnType: c.WeakTypeTag](c: Context)(handler: c.Expr[T]) = {
    import c.universe._

    buildSubmissionCopyTree[T, FieldList, ReturnType](c)(handler) { formTypeParameters =>
      q"""
      startingForm.copy[..$formTypeParameters](
        successHandlers = handler :: startingForm.successHandlers
      )
      """
    }
  }

  def onSubmissionImpl[T: c.WeakTypeTag, FieldList: c.WeakTypeTag, ReturnType: c.WeakTypeTag](c: Context)(handler: c.Expr[T]) = {
    import c.universe._

    buildSubmissionCopyTree[T, FieldList, ReturnType](c)(handler) { formTypeParameters =>
      q"""
      startingForm.copy[..$formTypeParameters](
        submissionHandlers = handler :: startingForm.submissionHandlers
      )
      """
    }
  }

  // Builds a tree to represent the copying of a handler into a FormalityForm's
  // submission handler. The construction of the actual copy part is left to the
  // caller, since that's where decisions about which handler we go into are
  // taken. Instead this function builds all the other stuff; when the
  // buildCopyExpression function is called, its returned tree is inlined
  // alongside:
  //  - A val startingForm, set to the FormalityForm expression before we add
  //    whatever handler we're about to add to it.
  //  - A val handler, set to the FieldList=>ReturnType handler generated from
  //    the handler expression.
  // It is also invoked with a Seq[Type] that represents the type parameters of
  // the startinForm.
  private def buildSubmissionCopyTree[
    T: c.WeakTypeTag,
    FieldList: c.WeakTypeTag,
    ReturnType: c.WeakTypeTag
  ](c: Context)
   (handler: c.Expr[T])
   (buildCopyExpression: (Seq[c.Type])=>c.Tree) =
  {
    import c.universe._

    val hlistType = c.weakTypeOf[FieldList]
    if (! (hlistType <:< c.weakTypeOf[HList])) {
      c.abort(c.enclosingPosition, "This macro only handles function=>HList stuff.")
    }

    val functionType = c.weakTypeOf[T]

    val hlistTypes = unwindHlistTypes(c)(c.weakTypeOf[FieldList])

    val functionTypes =
      functionType match {
        case TypeRef(_, _, parameterTypes :+ _) =>
          parameterTypes
        case TypeRef(_, _, Nil) =>
          c.abort(c.enclosingPosition,
            "Submission handlers must take field values as parameters.")
      }

    val formTypeParameters =
      c.prefix.actualType match {
        case TypeRef(_, _, args) =>
          args
      }

    val handlerAssignment =
      buildHandlerAssignment[T, ReturnType](c)(handler, hlistType, hlistTypes, functionTypes)

    q"""{
      val startingForm = ${c.prefix}
      $handlerAssignment

      ${buildCopyExpression(formTypeParameters)}
    }"""
  }

  private def buildHandlerAssignment[T, ReturnType : c.WeakTypeTag](
    c: Context
  )(
    handler: c.Expr[T],
    hlistType: c.Type,
    hlistTypes: Seq[c.Type],
    functionTypes: Seq[c.Type]
  ) = {
    import c.universe._

    // If we're dealing with an HList function, go the simple route.
    if (functionTypes.length == 1 && functionTypes.head <:< c.weakTypeOf[HList]) {
      q"val handler = $handler"
    } else {
      // Make sure types are compatible.
      if (hlistTypes.length != functionTypes.length) {
        c.abort(c.enclosingPosition,
          s"Function takes ${functionTypes.length} arguments, but form has " +
          s"${hlistTypes.length} fields.")
      }

      hlistTypes.zip(functionTypes).foreach {
        case (hlistType, functionType) =>
          if (! (hlistType <:< functionType)) {
            // TODO More precise error position.
            c.abort(c.enclosingPosition,
              s"Function takes $functionType, but field is of type $hlistType.")
          }
      }

      val (matcher, parameterList) =
        (0 until hlistTypes.length)
          .reverse
          .foldLeft((q"HNil": c.Tree, List[Ident]())) {
            case ((typeMatch, parameters), _) =>
              val name = TermName(c.freshName("fieldValue$"))

              (pq""":+:($name, $typeMatch)""", q"$name" :: parameters)
          }

      q"""
      val handler: Function1[${hlistType},${c.weakTypeOf[ReturnType]}] = {
        case $matcher =>
          ($handler).apply(..$parameterList)
      }
      """
    }
  }

  private[formality] def buildFormForTypes[
    FieldTypes <: HList : c.WeakTypeTag,
    FieldBoxTypes <: HList : c.WeakTypeTag,
    HeadFieldValueType : c.WeakTypeTag,
    RestFieldValueTypes <: HList : c.WeakTypeTag,
    ReturnType : c.WeakTypeTag
  ](c: Context)(formType: c.TypeName) = {
    import c.universe._

    val rootHlistType =
      c.weakTypeOf[FieldBoxTypes] match {
        case TypeRef(_, baseType, _) =>
          baseType
      }
    val fieldBoxTypeList = unwindHlistTypes(c)(c.weakTypeOf[FieldBoxTypes])
    val fieldValueTypeList =
      c.weakTypeOf[HeadFieldValueType] ::
        unwindHlistTypes(c)(c.weakTypeOf[RestFieldValueTypes])
    val fieldValueTypeListTree =
      AppliedTypeTree(
        Ident(rootHlistType),
        List(
          TypeTree(c.weakTypeOf[HeadFieldValueType]),
          TypeTree(c.weakTypeOf[RestFieldValueTypes])
        )
      )
    val returnTypeTree = TypeTree(c.weakTypeOf[ReturnType])

    // Both function types degrade to take HLists when we exceed the available
    // function arities.
    val functionType =
      if (fieldValueTypeList.length > 22) {
        AppliedTypeTree(
          Ident(TypeName("Function1")),
          List(fieldValueTypeListTree, returnTypeTree)
        )
      } else {
        AppliedTypeTree(
          Ident(TypeName("Function" + fieldBoxTypeList.length)),
          fieldValueTypeList.map { tpe => TypeTree(tpe) } :+ returnTypeTree
        )
      }
    val boxedFunctionType =
      if (fieldBoxTypeList.length > 22) {
        AppliedTypeTree(
          Ident(TypeName("Function1")),
          List(TypeTree(c.weakTypeOf[FieldBoxTypes]), returnTypeTree)
        )
      } else {
        AppliedTypeTree(
          Ident(TypeName("Function" + fieldBoxTypeList.length)),
          fieldBoxTypeList.map { tpe => TypeTree(tpe) } :+ returnTypeTree
        )
      }

    q"""{
      val formProto = ${c.prefix}

      new ${Ident(formType)}[
        ${c.weakTypeOf[FieldTypes]},
        ${c.weakTypeOf[FieldBoxTypes]},
        ${fieldValueTypeListTree},
        $functionType,
        $boxedFunctionType
      ](
        formProto.fields,
        () => {
          val (boxes, combinableBox) = formProto.computeValues()
          (boxes, combinableBox.rhs)
        },
        formProto.bindFields,
        Nil, Nil, Nil
      )
    }"""
  }

  private def unwindHlistTypes(c: Context)(hlistType: c.universe.Type):  List[c.universe.Type] = {
    import c.universe._

    hlistType match {
      case TypeRef(_, _, List(headType, rest)) =>
        headType :: unwindHlistTypes(c)(rest)
      case TypeRef(_, _, other) =>
        Nil
    }
  }
}

object StandardFormalityForm {
  import scala.reflect.macros.whitebox.Context

  def buildForm[
    FieldTypes <: HList : c.WeakTypeTag,
    FieldBoxTypes <: HList : c.WeakTypeTag,
    HeadFieldValueType : c.WeakTypeTag,
    RestFieldValueTypes <: HList : c.WeakTypeTag
  ](c: Context) = {
    FormalityFormHelper.buildFormForTypes[
      FieldTypes,
      FieldBoxTypes,
      HeadFieldValueType,
      RestFieldValueTypes,
      Unit
    ](c)(c.universe.TypeName("StandardFormalityForm"))
  }
}

case class StandardFormalityForm[
  FieldList <: HList,
  FieldBoxList <: HList,
  FieldValueList <: HList,
  FunctionType,
  BoxedFunctionType
](
  fields: FieldList,
  computeValues: ()=>(FieldBoxList, CombinableBoxie.Result[FieldValueList]),
  bindFields: CssSel,
  submissionHandlers: List[(FieldBoxList)=>Unit],
  successHandlers: List[(FieldValueList)=>Unit],
  failureHandlers: List[(List[Failure])=>Unit]
) extends FormalityForm[FieldList, FieldBoxList, FieldValueList, Unit, FunctionType, BoxedFunctionType] {
  def onSubmissionHList(handler: (FieldBoxList)=>Unit) = {
    copy[FieldList, FieldBoxList, FieldValueList, FunctionType, BoxedFunctionType](
      submissionHandlers = handler :: submissionHandlers
    )
  }
  def onSuccessHList(handler: (FieldValueList)=>Unit) = {
    copy[FieldList, FieldBoxList, FieldValueList, FunctionType, BoxedFunctionType](
      successHandlers = handler :: successHandlers
    )
  }
  def onFailure(handler: (List[Failure])=>Unit) = {
    copy[FieldList, FieldBoxList, FieldValueList, FunctionType, BoxedFunctionType](
      failureHandlers = handler :: failureHandlers
    )
  }

  def submitBind = {
    "type=submit" #> SHtml.onSubmit((s: String) => handleSubmit())
  }

  def handleSubmit() = {
    val (valueBoxes, valueResult) = computeValues()
    
    submissionHandlers.foreach(_(valueBoxes))

    valueResult match {
      case Right(values) =>
        successHandlers.foreach(_(values))
      case Left(failures) =>
        failureHandlers.foreach(_(failures))
    }
  }
}

object AjaxFormalityForm {
  import scala.reflect.macros.whitebox.Context

  def buildForm[
    FieldTypes <: HList : c.WeakTypeTag,
    FieldBoxTypes <: HList : c.WeakTypeTag,
    HeadFieldValueType : c.WeakTypeTag,
    RestFieldValueTypes <: HList : c.WeakTypeTag
  ](c: Context) = {
    FormalityFormHelper.buildFormForTypes[
      FieldTypes,
      FieldBoxTypes,
      HeadFieldValueType,
      RestFieldValueTypes,
      JsCmd
    ](c)(c.universe.TypeName("AjaxFormalityForm"))
  }
}
case class AjaxFormalityForm[
  FieldList <: HList,
  FieldBoxList <: HList,
  FieldValueList <: HList,
  FunctionType,
  BoxedFunctionType
](
  fields: FieldList,
  computeValues: ()=>(FieldBoxList, CombinableBoxie.Result[FieldValueList]),
  bindFields: CssSel,
  submissionHandlers: List[(FieldBoxList)=>Unit],
  successHandlers: List[(FieldValueList)=>JsCmd],
  failureHandlers: List[(List[Failure])=>JsCmd]
) extends FormalityForm[FieldList, FieldBoxList, FieldValueList, JsCmd, FunctionType, BoxedFunctionType] {
  def onSubmissionHList(handler: (FieldBoxList)=>Unit) = {
    copy[FieldList, FieldBoxList, FieldValueList, FunctionType, BoxedFunctionType](submissionHandlers = handler :: submissionHandlers)
  }
  def onSuccessHList(handler: (FieldValueList)=>JsCmd) = {
    copy[FieldList, FieldBoxList, FieldValueList, FunctionType, BoxedFunctionType](successHandlers = handler :: successHandlers)
  }
  def onFailure(handler: (List[Failure])=>JsCmd) = {
    copy[FieldList, FieldBoxList, FieldValueList, FunctionType, BoxedFunctionType](
      failureHandlers = handler :: failureHandlers
    )
  }

  def submitBind = {
    "type=submit" #> SHtml.ajaxOnSubmit(handleSubmit)
  }

  def handleSubmit() = {
    val (valueBoxes, valueResult) = computeValues()
    
    submissionHandlers.foreach(_(valueBoxes))

    valueResult match {
      case Right(values) =>
        successHandlers.map(_(values)).foldLeft(Noop)(_ & _)
      case Left(failures) =>
        failureHandlers.map(_(failures)).foldLeft(Noop)(_ & _)
    }
  }
}
