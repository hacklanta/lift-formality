package com.withoutincident
package formality

import net.liftweb.common.Box
import net.liftweb.http.SHtml
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
    EventHandlerType >: FieldValueType,
    SerializerType >: FieldValueType
  ](
    field: BaseFieldHolder[IncomingValueType, FieldValueType, ValidationType, EventHandlerType, SerializerType]
  ) = {
    this.copy[
      FieldHolderBase[FieldValueType] :: FieldList,
      Box[FieldValueType] :: FieldBoxList,
      FieldValueType :: FieldValueList
    ](fields = field :: fields)
  }

  def formalize[
    ReverseFieldList <: HList,
    ReverseFieldBoxList <: HList,
    ReverseFieldValueList <: HList
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
    // HACK ALERT
    // The below map/reverses let the compiler properly infer the
    // ReverseFieldBoxList and ReverseFieldValueList types to pass
    // them on to FormalityForm. But, we don't need (indeed, we don't
    // *want*) the actual operations to happen at runtime. So we
    // wrap them in an if false, and trust that the JVM will see that
    // and just discard the code altogether (and if not, simply skip
    // over it).
    if (false) {
      val reversedFieldBoxes = fields.map(mapFieldBox).reverse
      val reversedFieldValues = fields.map(mapFieldValue_!).reverse
    }

    FormalityForm[
      ReverseFieldList,
      ReverseFieldBoxList,
      ReverseFieldValueList
    ](reversedFields, Nil, Nil, Nil)
  }
}

case class FormalityForm[
  FieldList <: HList : *->*[FieldHolderBase]#λ,
  FieldBoxList <: HList : *->*[Box]#λ,
  FieldValueList <: HList
](
  fields: FieldList,
  submissionHandlers: List[(FieldBoxList)=>Unit],
  successHandlers: List[(FieldValueList)=>Unit],
  failureHandlers: List[(FieldBoxList)=>Unit]
) {
  import FormalityForm._

  def onSubmission(handler: (FieldBoxList)=>Unit) = {
    copy[FieldList, FieldBoxList, FieldValueList](submissionHandlers = handler :: submissionHandlers)
  }
  def onSuccess(handler: (FieldValueList)=>Unit) = {
    copy[FieldList, FieldBoxList, FieldValueList](successHandlers = handler :: successHandlers)
  }
  def onFailure(handler: (FieldBoxList)=>Unit) = {
    copy[FieldList, FieldBoxList, FieldValueList](failureHandlers = handler :: failureHandlers)
  }

  def binder()(
    implicit fieldFolder: LeftFolderAux[FieldList, CssSel, combineFieldBinders.type, CssSel],
             definedCheckFolder: LeftFolderAux[FieldList, Boolean, checkFieldsDefined.type, Boolean],
             boxFolder: RightFolderAux[FieldList, HNil, extractFieldBox.type, FieldBoxList],
             valueFolder: RightFolderAux[FieldList, HNil, extractFieldValue.type, FieldValueList]
  ): CssSel = {
    val submitBind = "type=submit" #> SHtml.ajaxOnSubmit(() => handleSubmit())
    val fieldBinds = fields.foldLeft(submitBind)(combineFieldBinders)

    fieldBinds
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
