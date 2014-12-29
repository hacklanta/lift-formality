package com.hacklanta
package formality

import scala.language.experimental.macros

import net.liftweb.common.Box
import net.liftweb.http.SHtml
import net.liftweb.http.js.JsCmd
import net.liftweb.http.js.JsCmds._
import net.liftweb.util._
  import Helpers._

import shapeless._
  import HList._
  import UnaryTCConstraint._

object FormalityForm {
  // shapeless helper to map the HList of FieldHolder[T]s into an HList
  // of their boxed values. Remember the FieldHolder[T]s do not
  // necessarily share the same type T. This is used to take an HList of
  // FieldHolder[T]s and map it to an HList of Box[T]s with
  // heterogeneous Ts.
  object mapFieldBox extends Poly1 {
    implicit def caseHListField[FieldValueType] =
      at[FieldHolderBase[FieldValueType]] { field =>
        field.value
      }
  }
  // shapeless helper to map the HList of FieldHolder[T]s into an HList
  // of their extracted values. Remember the FieldHolder[T]s do not
  // necessarily share the same type T. This is used to take an HList of
  // FieldHolder[T]s and map it to an HList of Ts with heterogeneous Ts.
  //
  // NOTE: This is not meant to actually run! It uses
  // openOrThrowException, but is currently used specifically to help the
  // type inferencer infer the reversed type of the HList used in the forms.
  // More on that in the FormalityFormProto documentation. Don't use
  // this unless you are ready for exceptions.
  object mapFieldValue_! extends Poly1 {
    implicit def caseHListField[FieldValueType] =
      at[FieldHolderBase[FieldValueType]] { field =>
        field.value openOrThrowException "This should seriously not be running. Hide somewhere safe and call 911."
      }
  }
  object combineFieldBinders extends Poly2 {
    implicit def caseBoolField[FieldValueType] =
      at[CssSel, FieldHolderBase[FieldValueType]] { (soFar, field) =>
        soFar & field.binder
      }
  }
  object checkFieldsDefined extends Poly2 {
    implicit def caseBoolField[FieldValueType] =
      at[Boolean, FieldHolderBase[FieldValueType]] { (soFar, field) =>
        soFar && field.value.isDefined
      }
  }
  object extractFieldValue extends Poly2 {
    implicit def caseHListField[FieldValueType, RemainingFieldList <: HList] =
      at[FieldHolderBase[FieldValueType], RemainingFieldList] { (field, list) =>
        field.value.openOrThrowException("This call was not wrapped in trick-nasty checkFieldsDefined goodness.") :: list
      }
  }
  object extractFieldBox extends Poly2 {
    implicit def caseHListField[FieldValueType, RemainingFieldList <: HList] =
      at[FieldHolderBase[FieldValueType], RemainingFieldList] { (field, list) =>
        field.value :: list
      }
  }
}

object FormalityFormProto {
  import scala.reflect.macros.whitebox.Context

  private def buildFieldsExpression(c: Context)(fields: Seq[c.Expr[FieldHolderBase[_]]]) = {
    import c.universe._

    fields.foldLeft(q"""${c.prefix}""") { (expression, field) =>
      q"""${expression}.withField($field)"""
    }
  }

  private def withFieldsHelper(c: Context)(fields: Seq[c.Expr[FieldHolderBase[_]]], formalizeCall: c.universe.TermName) = {
    import c.universe._

    // Split out into two expressions as otherwise the resulting expression gets
    // the compiler confused on types.
    q"""{
      val formWithFields = ${buildFieldsExpression(c)(fields)}
      formWithFields.reverse().${formalizeCall}
    }"""
  }

  def withFieldsImpl(c: Context)(fields: c.Expr[FieldHolderBase[_]]*) = {
    withFieldsHelper(c)(fields, c.universe.TermName("formalize"))
  }

  def withAjaxFieldsImpl(c: Context)(fields: c.Expr[FieldHolderBase[_]]*) = {
    withFieldsHelper(c)(fields, c.universe.TermName("ajaxFormalize"))
  }
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
case class ReversedFormalityFormProto[
  FieldList <: HList : *->*[FieldHolderBase]#λ,
  FieldBoxList <: HList : *->*[Box]#λ,
  FieldValueList <: HList
](
  fields: FieldList
) {
  def formalize: StandardFormalityForm[FieldList, FieldBoxList, FieldValueList, _, _] = macro StandardFormalityForm.buildForm[FieldList, FieldBoxList, FieldValueList]
  def ajaxFormalize: AjaxFormalityForm[FieldList, FieldBoxList, FieldValueList, _, _] = macro AjaxFormalityForm.buildForm[FieldList, FieldBoxList, FieldValueList]
}
case class FormalityFormProto[
  FieldList <: HList : *->*[FieldHolderBase]#λ,
  FieldBoxList <: HList : *->*[Box]#λ,
  FieldValueList <: HList
](
  fields: FieldList
) {
  import FormalityForm._

  def withField[
    IncomingValueType,
    FieldValueType,
    ValidationType >: FieldValueType,
    EventHandlerType >: FieldValueType
  ](
    field: BaseFieldHolder[IncomingValueType, FieldValueType, ValidationType, EventHandlerType]
  ) = {
    this.copy[
      FieldHolderBase[FieldValueType] :: FieldList,
      Box[FieldValueType] :: FieldBoxList,
      FieldValueType :: FieldValueList
    ](fields = field :: fields)
  }

  def withFields(fields: FieldHolderBase[_]*): FormalityForm[_, _, _, _, _, _] = macro FormalityFormProto.withFieldsImpl
  def withAjaxFields(fields: FieldHolderBase[_]*): FormalityForm[_, _, _, _, _, _] = macro FormalityFormProto.withAjaxFieldsImpl

  // First we reverse, then we formalize.
  def reverse[
    ReverseFieldList <: HList,
    ReverseFieldBoxList <: HList,
    ReverseFieldValueList <: HList,
    FunctionType,
    BoxedFunctionType
  ]()(
    implicit fieldReverser: ReverseAux[FieldList, ReverseFieldList],
             boxMapper: MapperAux[mapFieldBox.type, FieldList, FieldBoxList],
             fieldBoxReverser: ReverseAux[FieldBoxList, ReverseFieldBoxList],
             valueMapper: MapperAux[mapFieldValue_!.type, FieldList, FieldValueList],
             fieldValueReverser: ReverseAux[FieldValueList, ReverseFieldValueList],
             reverseFieldListConstraint: UnaryTCConstraint[ReverseFieldList, FieldHolderBase],
             reverseFieldBoxListConstraint: UnaryTCConstraint[ReverseFieldBoxList, Box]
  ) = {
    val reversedFields = fields.reverse

    ReversedFormalityFormProto[
      ReverseFieldList,
      ReverseFieldBoxList,
      ReverseFieldValueList
    ](reversedFields)
  }
}

abstract class FormalityForm[
  FieldList <: HList : *->*[FieldHolderBase]#λ,
  FieldBoxList <: HList : *->*[Box]#λ,
  FieldValueList <: HList,
  RequestResultType,
  FunctionType,
  BoxedFunctionType
] {
  import FormalityForm._

  def onSubmission(handler: BoxedFunctionType): StandardFormalityForm[FieldList, FieldBoxList, FieldValueList, FunctionType, BoxedFunctionType] = macro FormalityFormHelper.onSubmissionImpl[BoxedFunctionType, FieldBoxList, RequestResultType]
  def onSubmissionHList(handler: (FieldBoxList)=>Unit): FormalityForm[FieldList,FieldBoxList,FieldValueList,RequestResultType, FunctionType, BoxedFunctionType]

  def onSuccess(handler: FunctionType): FormalityForm[FieldList,FieldBoxList,FieldValueList,RequestResultType, FunctionType, BoxedFunctionType] = macro FormalityFormHelper.onSuccessImpl[FunctionType, FieldValueList, RequestResultType]
  def onSuccessHList(handler: (FieldValueList)=>RequestResultType): FormalityForm[FieldList,FieldBoxList,FieldValueList,RequestResultType, FunctionType, BoxedFunctionType]

  def onFailure(handler: BoxedFunctionType): StandardFormalityForm[FieldList, FieldBoxList, FieldValueList, FunctionType, BoxedFunctionType] = macro FormalityFormHelper.onFailureImpl[BoxedFunctionType, FieldBoxList, RequestResultType]
  def onFailureHList(handler: (FieldBoxList)=>RequestResultType): FormalityForm[FieldList,FieldBoxList,FieldValueList,RequestResultType, FunctionType, BoxedFunctionType]

  def fields: FieldList
  def submitBind(
    implicit definedCheckFolder: LeftFolderAux[FieldList, Boolean, checkFieldsDefined.type, Boolean],
             boxFolder: RightFolderAux[FieldList, HNil, extractFieldBox.type, FieldBoxList],
             valueFolder: RightFolderAux[FieldList, HNil, extractFieldValue.type, FieldValueList]
  ): CssSel

  def binder()(
    implicit fieldFolder: LeftFolderAux[FieldList, CssSel, combineFieldBinders.type, CssSel],
             definedCheckFolder: LeftFolderAux[FieldList, Boolean, checkFieldsDefined.type, Boolean],
             boxFolder: RightFolderAux[FieldList, HNil, extractFieldBox.type, FieldBoxList],
             valueFolder: RightFolderAux[FieldList, HNil, extractFieldValue.type, FieldValueList]
  ): CssSel = {
    val fieldBinds = fields.foldLeft(submitBind)(combineFieldBinders)

    fieldBinds
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

  def onFailureImpl[T: c.WeakTypeTag, FieldList: c.WeakTypeTag, ReturnType: c.WeakTypeTag](c: Context)(handler: c.Expr[T]) = {
    import c.universe._

    buildSubmissionCopyTree[T, FieldList, ReturnType](c)(handler) { formTypeParameters =>
      q"""
      startingForm.copy[..$formTypeParameters](
        failureHandlers = handler :: startingForm.failureHandlers
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

      val matcher =
        (0 until hlistTypes.length).reverse.foldLeft(q"HNil": c.Tree) { (typeMatch, i) =>
          pq""":+:(${TermName("thingie" + i)}, $typeMatch)"""
        }

      val parameterList =
        (0 until hlistTypes.length).map { (i) =>
          q"""${TermName("thingie" + i)}"""
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
    FieldValueTypes <: HList : c.WeakTypeTag,
    ReturnType : c.WeakTypeTag
  ](c: Context)(formType: c.TypeName) = {
    import c.universe._

    val fieldBoxTypeList = unwindHlistTypes(c)(c.weakTypeOf[FieldBoxTypes])
    val fieldValueTypeList = unwindHlistTypes(c)(c.weakTypeOf[FieldValueTypes])

    val returnTypeTree = TypeTree(c.weakTypeOf[ReturnType])

    // Both function types degrade to take HLists when we exceed the available
    // function arities.
    val functionType =
      if (fieldValueTypeList.length > 22) {
        AppliedTypeTree(
          Ident(TypeName("Function1")),
          List(TypeTree(c.weakTypeOf[FieldValueTypes]), returnTypeTree)
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
      val reversedForm = ${c.prefix}

      new ${Ident(formType)}[
        ${c.weakTypeOf[FieldTypes]},
        ${c.weakTypeOf[FieldBoxTypes]},
        ${c.weakTypeOf[FieldValueTypes]},
        $functionType,
        $boxedFunctionType
      ](reversedForm.fields, Nil, Nil, Nil)
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
    FieldValueTypes <: HList : c.WeakTypeTag
  ](c: Context) = {
    FormalityFormHelper.buildFormForTypes[
      FieldTypes,
      FieldBoxTypes,
      FieldValueTypes,
      Unit
    ](c)(c.universe.TypeName("StandardFormalityForm"))
  }
}

case class StandardFormalityForm[
  FieldList <: HList : *->*[FieldHolderBase]#λ,
  FieldBoxList <: HList : *->*[Box]#λ,
  FieldValueList <: HList,
  FunctionType,
  BoxedFunctionType
](
  fields: FieldList,
  submissionHandlers: List[(FieldBoxList)=>Unit],
  successHandlers: List[(FieldValueList)=>Unit],
  failureHandlers: List[(FieldBoxList)=>Unit]
) extends FormalityForm[FieldList, FieldBoxList, FieldValueList, Unit, FunctionType, BoxedFunctionType] {
  import FormalityForm._

  def submitBind(
    implicit definedCheckFolder: LeftFolderAux[FieldList, Boolean, checkFieldsDefined.type, Boolean],
                              boxFolder: RightFolderAux[FieldList, HNil, extractFieldBox.type, FieldBoxList],
                              valueFolder: RightFolderAux[FieldList, HNil, extractFieldValue.type, FieldValueList]
  ) = {
    "type=submit" #> SHtml.onSubmit((s: String) => handleSubmit())
  }

  def onSubmissionHList(handler: (FieldBoxList)=>Unit) = {
    copy[FieldList, FieldBoxList, FieldValueList, FunctionType, BoxedFunctionType](submissionHandlers = handler :: submissionHandlers)
  }
  def onSuccessHList(handler: (FieldValueList)=>Unit) = {
    copy[FieldList, FieldBoxList, FieldValueList, FunctionType, BoxedFunctionType](successHandlers = handler :: successHandlers)
  }
  def onFailureHList(handler: (FieldBoxList)=>Unit) = {
    copy[FieldList, FieldBoxList, FieldValueList, FunctionType, BoxedFunctionType](failureHandlers = handler :: failureHandlers)
  }

  def handleSubmit()(implicit definedCheckFolder: LeftFolderAux[FieldList, Boolean, checkFieldsDefined.type, Boolean],
                              boxFolder: RightFolderAux[FieldList, HNil, extractFieldBox.type, FieldBoxList],
                              valueFolder: RightFolderAux[FieldList, HNil, extractFieldValue.type, FieldValueList]) = {
    val boxes: FieldBoxList = fields.foldRight(HNil: HNil)(extractFieldBox)
    submissionHandlers.foreach(_(boxes))

    // FIXME We should be able to do this with a liftO-like function
    // FIXME that lifts a list of Boxes into an Box of HList with
    // FIXME the appropriate type, rather than checking for definition
    // FIXME and then extracting the value via openOrThrowException.
    if (fields.foldLeft(true)(checkFieldsDefined)) {
      val values: FieldValueList = fields.foldRight(HNil: HNil)(extractFieldValue)

      successHandlers.foreach(_(values))
    } else {
      failureHandlers.foreach(_(boxes))
    }
  }
}

object AjaxFormalityForm {
  import scala.reflect.macros.whitebox.Context

  def buildForm[
    FieldTypes <: HList : c.WeakTypeTag,
    FieldBoxTypes <: HList : c.WeakTypeTag,
    FieldValueTypes <: HList : c.WeakTypeTag
  ](c: Context) = {
    FormalityFormHelper.buildFormForTypes[
      FieldTypes,
      FieldBoxTypes,
      FieldValueTypes,
      JsCmd
    ](c)(c.universe.TypeName("AjaxFormalityForm"))
  }
}
case class AjaxFormalityForm[
  FieldList <: HList : *->*[FieldHolderBase]#λ,
  FieldBoxList <: HList : *->*[Box]#λ,
  FieldValueList <: HList,
  FunctionType,
  BoxedFunctionType
](
  fields: FieldList,
  submissionHandlers: List[(FieldBoxList)=>Unit],
  successHandlers: List[(FieldValueList)=>JsCmd],
  failureHandlers: List[(FieldBoxList)=>JsCmd]
) extends FormalityForm[FieldList, FieldBoxList, FieldValueList, JsCmd, FunctionType, BoxedFunctionType] {
  import FormalityForm._

  def submitBind(
    implicit definedCheckFolder: LeftFolderAux[FieldList, Boolean, checkFieldsDefined.type, Boolean],
                              boxFolder: RightFolderAux[FieldList, HNil, extractFieldBox.type, FieldBoxList],
                              valueFolder: RightFolderAux[FieldList, HNil, extractFieldValue.type, FieldValueList]
  ) = {
    "type=submit" #> SHtml.ajaxOnSubmit(() => handleSubmit())
  }

  def onSubmissionHList(handler: (FieldBoxList)=>Unit) = {
    copy[FieldList, FieldBoxList, FieldValueList, FunctionType, BoxedFunctionType](submissionHandlers = handler :: submissionHandlers)
  }
  def onSuccessHList(handler: (FieldValueList)=>JsCmd) = {
    copy[FieldList, FieldBoxList, FieldValueList, FunctionType, BoxedFunctionType](successHandlers = handler :: successHandlers)
  }
  def onFailureHList(handler: (FieldBoxList)=>JsCmd) = {
    copy[FieldList, FieldBoxList, FieldValueList, FunctionType, BoxedFunctionType](failureHandlers = handler :: failureHandlers)
  }

  def handleSubmit()(implicit definedCheckFolder: LeftFolderAux[FieldList, Boolean, checkFieldsDefined.type, Boolean],
                              boxFolder: RightFolderAux[FieldList, HNil, extractFieldBox.type, FieldBoxList],
                              valueFolder: RightFolderAux[FieldList, HNil, extractFieldValue.type, FieldValueList]) = {
    val boxes: FieldBoxList = fields.foldRight(HNil: HNil)(extractFieldBox)
    submissionHandlers.foreach(_(boxes))

    // FIXME We should be able to do this with a liftO-like function
    // FIXME that lifts a list of Boxes into an Box of HList with
    // FIXME the appropriate type, rather than checking for definition
    // FIXME and then extracting the value via openOrThrowException.
    if (fields.foldLeft(true)(checkFieldsDefined)) {
      val values: FieldValueList = fields.foldRight(HNil: HNil)(extractFieldValue)

      successHandlers.map(_(values)).foldLeft(Noop)(_ & _)
    } else {
      failureHandlers.map(_(boxes)).foldLeft(Noop)(_ & _)
    }
  }
}
