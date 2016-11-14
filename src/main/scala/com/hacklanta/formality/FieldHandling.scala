package com.hacklanta
package formality

import scala.xml._

import net.liftweb.common._
import net.liftweb.http._
  import js._
    import JsCmds._
  import SHtml._
import net.liftweb.util._
  import Helpers._

// A container for a given field's value. This is a TransientRequestVar, and it
// is salted so that each instance is mapped uniquely in the function map.
private[formality] class FieldValueVar[FieldValueType](dflt: =>Box[FieldValueType] = Empty) extends TransientRequestVar[Box[FieldValueType]](dflt) {
  override def __nameSalt = randomString(10)
}
// A container for a given field's name. This is a RequestVar, and it is salted
// so that each instance is mapped uniquely in the function map.
private[formality] class FieldNameVar(dflt: =>String) extends RequestVar[String](dflt) {
  override def __nameSalt = randomString(10)
}

/**
 * A FieldHolder, at its most basic, has a value that is a Box of its
 * value type and can provide a CSS selector to transform an HTML
 * template annotating a form field with the appropriate information to
 * extract that value during a form submission.
 */
trait FieldHolderBase[FieldValueType] {
  /**
   * The value of this field holder; three value possibilities:
   *  - If it is Empty, the relevant field either wasn't submitted
   *    during this request, or it hasn't yet been processed.
   *  - If it is a Failure, either validation or value conversion (from
   *    String to the FieldValueType) failed. In case of a validation
   *    failure, this should be a ParamFailure whose param is the list of
   *    validation failures (a List[String])
   *
   * - If it is Full, it contains the value for the current form
   *   submission.
   */
  def value: Box[FieldValueType]
  /**
   * Provides a CSS Selector Transform that can bind the appropriate
   * attributes to a form field to ensure its submission in a form will
   * result in the processing of the submitted data.
   */
  def binder: CssSel
}

/**
 * FieldHolder represents a form field that processes a value of type
 * FieldValueType, and that can run a set of validations on the incoming
 * field value when it is submitted. It can also attach event handler
 * callbacks that call back to the server with the current inputted
 * value when certain events occur on the client.
 *
 * This base provides about 80% of the structure that most fields will
 * share. It provides an overridable handler function that will handle
 * the field when it is submitted in a form. This can be overridden by
 * child classes for e.g. file handling.
 *
 * @param selector The CSS selector that identifies the field we'll be
 * binding to in the template. Note that the minimum amount of changes
 * are made to that field. For example, the type of the field
 * (type="string", number, etc) is left up to the template.
 *
 * @param initialValue The initial value of the form field. This will be
 * serialized using the valueSeralizer.
 *
 * @param valueConverter A function that converts from a String to a
 * Box[T]. If the value conversion function returns a Failure, the
 * associated failure message is sent down as an error with the field to
 * the client. If it returns an Empty, a generic message is sent down as
 * an error. This should be implicitly resolved by the compiler unless
 * you are creating a field for a type that doesn't have a
 * pre-configured value converter. In that case you will have to provide
 * the converter yourself. See dateValueConverter as a quick example.
 *
 * @param eventHandlerValueConverter In most cases the same as the
 * valueConverter. File uplaods, as one example of when this isn't the
 * case, convert a FileParamHolder, but their event handlers still get a
 * String.
 */
abstract class BaseFieldHolder[
  // The type of the value incoming from Lift. Typically a String, but
  // certain cases can arise, like file upload, where it may be
  // something different.
  IncomingValueType,
  FieldValueType,
  // for example, a Square value type should be able to use a Validation[Shape]
  ValidationType >: FieldValueType,
  // for example, a Square value type should be able to use a EventHandler[Rectangle]
  EventHandlerType >: FieldValueType
](
  selector: String,
  initialValue: Box[FieldValueType],
  validations: List[Validation[ValidationType]],
  boxedValidations: List[Validation[Box[ValidationType]]],
  eventHandlers: List[EventHandler[EventHandlerType]]
)(
  implicit eventHandlerValueConverter: (String)=>Box[FieldValueType]
) extends FieldHolderBase[FieldValueType] {
  protected def convertValue(incomingValue: IncomingValueType): Box[FieldValueType]
  protected def serializeValue(value: FieldValueType): String

  /**
   * This should return a copy of this BaseFieldHolder with the
   * specified validation attached. Left abstract because by far
   * the best implementation is using a case class copy method.
   */
  def validatingWith(validation: Validation[ValidationType]): BaseFieldHolder[IncomingValueType, FieldValueType, ValidationType, EventHandlerType]
  /**
   * Adds the given validation to the list of validations run on this
   * field at form processing time. Note that validations may add
   * attributes to the field to indicate on the client side what types of
   * validations are expected; see the Validation trait for more.
   *
   * Note that FieldHolders are immutable; this returns a copy of this
   * FieldHolder with an updated validation list.
   *
   * Aliased as `[[validatingWith(validation* validatingWith]]` for folks who
   * don't like operators.
   */
  def ?(validation: Validation[ValidationType]) = validatingWith(validation)

  /**
   * This should return a copy of this BaseFieldHolder with the
   * specified validation attached. Left abstract because by far
   * the best implementation is using a case class copy method.
   */
  def validatingWith(boxedValidation: Validation[Box[ValidationType]])(implicit dummy: DummyImplicit): BaseFieldHolder[IncomingValueType, FieldValueType, ValidationType, EventHandlerType]
  /**
   * Adds the given boxed validation to the list of validations run on this
   * field at form processing time. Note that validations may add
   * attributes to the field to indicate on the client side what types of
   * validations are expected; see the `[[Validation]]` trait for more.
   *
   * Note that `FieldHolder`s are immutable; this returns a copy of this
   * `FieldHolder` with an updated validation list.
   *
   * Aliased as `[[[validatingWith(boxed* validatingWith]]]` for folks who don't
   * like operators.
   */
  def ?(boxedValidation: Validation[Box[ValidationType]])(implicit dummy: DummyImplicit) = validatingWith(boxedValidation)

  /**
   * This should return a copy of this `BaseFieldHolder` with the
   * specified event handler attached. Left abstract because by far the
   * best implementation is using a case class copy method.
   */
  def handlingEvent(eventHandler: EventHandler[EventHandlerType]): BaseFieldHolder[IncomingValueType, FieldValueType, ValidationType, EventHandlerType]
  /**
   * Adds the given event handler to the list of event handlers this field
   * will have on the client. See EventHandler for more. Meant to be
   * used with Formality.on for fluency (e.g., `field -> on("change", checkStuff _)`).
   *
   * Note that `FieldHolder`s are immutable; this returns a copy of this
   * `FieldHolder` with an updated event handler list.
   *
   * Aliased as `[[handlingEvent]]` for folks who don't like operators.
   */
  def ->(eventHandler: EventHandler[EventHandlerType]) = handlingEvent(eventHandler)

  /**
   * The handler function should set this `TransientRequestVar` to the
   * appropriate value.
   */
  protected val fieldValue = new FieldValueVar[FieldValueType]()
  /**
   * This is the final computed field value after all validations have been
   * handled. It memoizes the result of `computeFieldValue`.
   */
  protected val computedFieldValue = new FieldValueVar[FieldValueType](computeFieldValue)
  /**
   * Stores the field name generated by `generateFunctionIdAndHandler`. The
   * current field name should be accessed from here rather than directly
   * through `generateFunctionIdAndHandler` except in case of clear preference.
   */
  protected val fieldName = new FieldNameVar(generateFunctionIdAndHandler)

  /**
   * Returns the current value of the field. If no value was seen in the last
   * form submission, invokes `boxedValidations` and registers any resulting
   * validation errors.
   */
  def value = computedFieldValue.is

  /**
   * Computes the final field value and applies any validations. Ensures
   * validations only run once no matter how many times the field value is
   * accessed.
   */
  protected def computeFieldValue = {
    fieldValue.is match {
      case Empty =>
        val validationErrors = boxedValidations.reverse.flatMap(_(Empty))

        addValidationErrors(validationErrors: _*)

        Failure("Empty field failed validation.") ~> validationErrors
      case other =>
        other
    }
  }

  /**
   * Adds the passed error messages, using `S.error`. Can be overridden to use
   * something other than `S.error`.
   */
  protected def addValidationErrors(errorMessages: String*): Unit = {
    for (errorMessage <- errorMessages) {
      S.error(fieldName.is, errorMessage)
    }
  }

  /**
   * Creates a handler function, maps it (potentially using `S.fmapFunc`),
   * and returns the resulting function id to bind to the HTML
   * field. See the implementation in `SimpleFieldHolder` for a sample.
   */
  protected def generateFunctionIdAndHandler: String
  /**
   * Defines the base transform that adds the function id and initial
   * value to the HTML field specified in the selector parameter. When
   * overriding, it is highly suggested that you call
   * super.baseTransform and append your own transforms to that.
   */
  protected def baseTransform: CssSel = {
    val functionId = fieldName.is

    val nameTransform = (selector + " [name]") #> functionId

    initialValue.map { startValue =>
      nameTransform &
      (selector + " [value]") #> serializeValue(startValue)
    } getOrElse {
      nameTransform
    }
  }

  /**
   * Provides the CSS transform that transforms an input field in the
   * template into one that will be processed by this field's value
   * conversion and validation functions, that will have the provided
   * initial value, and that will run this field's associated callbacks
   * for the event handlers it has specified.
   *
   * If you override the binder, please make sure you properly
   * map your handler function, event handlers, and validations. This
   * binder automatically calls `[[generateFunctionIdAndHandler]]` and binds
   * its result to the name of the HTML field, and your code must do
   * something similar to ensure the server-side function you define is
   * called on form submission.
   */
  def binder: CssSel = {
    val withValidations = validations.foldLeft(baseTransform)(_ & _.binder(selector))

    eventHandlers.foldLeft(withValidations)(_ & _.binder(selector, eventHandlerValueConverter))
  }
}
/**
 * This case class creates a field holder for a simple field that gets a
 * String from the client.
 */
case class SimpleFieldHolder[
  FieldValueType,
  ValidationType >: FieldValueType,
  EventHandlerType >: FieldValueType
](
  selector: String,
  initialValue: Box[FieldValueType],
  validations: List[Validation[ValidationType]],
  boxedValidations: List[Validation[Box[ValidationType]]],
  eventHandlers: List[EventHandler[EventHandlerType]]
)(
  implicit valueConverter: (String)=>Box[FieldValueType],
           valueSerializer: (FieldValueType)=>String
) extends BaseFieldHolder[String, FieldValueType, ValidationType, EventHandlerType](
            selector, initialValue, validations, boxedValidations, eventHandlers
          ) {
  protected def convertValue(incomingValue: String) = valueConverter(incomingValue)
  protected def serializeValue(value: FieldValueType) = valueSerializer(value)

  def validatingWith(validation: Validation[ValidationType]) = {
    this.copy(validations = validation :: validations)
  }
  def validatingWith(boxedValidation: Validation[Box[ValidationType]])(implicit dummy: DummyImplicit) = {
    this.copy(boxedValidations = boxedValidation :: boxedValidations)
  }
  def handlingEvent(eventHandler: EventHandler[EventHandlerType]) = {
    this.copy(eventHandlers = eventHandler :: eventHandlers)
  }

  protected def generateFunctionIdAndHandler: String = {
    // Awkward super-dirty, but we need to reference the function id in
    // the handler, and we only get the function id after mapping the
    // handler, so we have to go this route. As long as you don't run
    // the handler before the fmapFunc runs, everything is peachy.
    //
    // WARNING: DON'T RUN THE HANDLER BEFORE THE FMAPFUNC RUNS.
    //
    // Kword? ;)
    var functionId: String = null

    def handler(incomingValue: String): Unit = {
      convertValue(incomingValue) match {
        case Full(convertedValue) =>
          val validationErrors =
            validations.reverse.flatMap(_(convertedValue)) ++
            boxedValidations.reverse.flatMap(_(Full(convertedValue)))

          addValidationErrors(validationErrors: _*)

          if (validationErrors.isEmpty) {
            fieldValue(Full(convertedValue))
          } else {
            fieldValue(Failure(convertedValue + " failed validations.") ~> validationErrors)
          }

        case failure @ Failure(failureError, _, _) =>
          val validationErrors = boxedValidations.reverse.flatMap(_(failure))

          fieldValue(failure ~> incomingValue)
          addValidationErrors(failureError)

        case Empty =>
          fieldValue(Failure("Unrecognized response.") ~> incomingValue)
          addValidationErrors("Unrecognized response.")
      }
    }

    functionId = S.fmapFunc(handler _)(funcName => funcName)

    functionId
  }
}
/**
 * This case class creates a field holder for a select field that gets a
 * `String` from the client. If `asRadioButtons` is `true`, the binder created
 * for this select field is designed to bind to radio buttons and their
 * labels instead of producing a select field.
 *
 * When dealing with `select` elements (i.e., `asRadioButtons` is `false`),
 * the whole element specified by `selector` will be replaced by a new `select`
 * element.
 *
 * When `asRadioButtons` is `true`, the element specified by `selector` will be
 * repeated once for each of the passed `options`. The option label will be
 * used to set the text of the `label`. If the radio button is nested in the
 * `label`, it will be left at the end of the `label`. If a given
 * `SelectableOption` specifies an `id` attribute, in addition to that
 * attribute being set on the radio button, the `for` attribute of the `label`
 * element (if present) will be set to the same value.
 */
case class SelectFieldHolder[
  FieldValueType,
  ValidationType >: FieldValueType,
  EventHandlerType >: FieldValueType
](
  selector: String,
  initialValue: Box[FieldValueType],
  options: List[SelectableOption[FieldValueType]],
  validations: List[Validation[ValidationType]],
  boxedValidations: List[Validation[Box[ValidationType]]],
  eventHandlers: List[EventHandler[EventHandlerType]],
  asRadioButtons: Boolean
) extends BaseFieldHolder[String, FieldValueType, ValidationType, EventHandlerType](
            selector, initialValue, validations, boxedValidations, eventHandlers
          )(
            eventHandlerValueConverter = { eventHandlingValue: String => Empty /* we don't get file values for event handlers */ }
          ) {
  val (noncedOptions, defaultNonce, optionProcessor) = {
    final case class SelectableOptionWithNonce[+T](value: T, nonce: String, label: String, attrs: ElemAttr*)

    val secure = options.map { selectableOption =>
      SelectableOptionWithNonce(selectableOption.value, randomString(20), selectableOption.label, selectableOption.attrs: _*)
    }

    val defaultNonce = initialValue.flatMap { default =>
      secure.find(_.value == default).map(_.nonce)
    }

    val nonces = secure.map { selectableOptionWithNonce =>
      SelectableOption(selectableOptionWithNonce.nonce, selectableOptionWithNonce.label, selectableOptionWithNonce.attrs: _*)
    }

    def process(nonce: String): Box[FieldValueType] = secure.find(_.nonce == nonce).map(_.value)

    (nonces, defaultNonce, process _)
  }

  protected def convertValue(incomingValue: String) = {
    optionProcessor(incomingValue) ?~ "Unrecognized entry."
  }
  // We don't actually use this directly; instead, we set up the default value above.
  protected def serializeValue(value: FieldValueType) = ""

  def validatingWith(validation: Validation[ValidationType]) = {
    this.copy(validations = validation :: validations)
  }
  def validatingWith(boxedValidation: Validation[Box[ValidationType]])(implicit dummy: DummyImplicit) = {
    this.copy(boxedValidations = boxedValidation :: boxedValidations)
  }
  def handlingEvent(eventHandler: EventHandler[EventHandlerType]) = {
    this.copy(eventHandlers = eventHandler :: eventHandlers)
  }

  override val binder: CssSel = {
    if (asRadioButtons) {
      radioButtonBinder
    } else {
      selectBinder
    }
  }

  def radioButtonBinder = {
    val functionId = fieldName.is

    val withValidations = validations.foldLeft("nothing" #> PassThru)(_ & _.binder(selector))
    val validationsAndEvents = eventHandlers.foldLeft(withValidations)(_ & _.binder(selector, optionProcessor))

    selector #> noncedOptions.map { option =>
      val radioBinder =
        "type=radio [name]" #> functionId &
        "type=radio [value]" #> option.value &
        "type=radio [selected]" #> Some("selected").filter(_ => Full(option.value) == defaultNonce) &
        "type=radio" #> validationsAndEvents

      val (idAttribute, radioBinderWithAttributes) =
        option.attrs.foldLeft((None: Option[String], radioBinder)) { (binderSoFar, attribute) =>
          attribute match {
            case BasicElemAttr(name, value) =>
              val updatedBinder =
                binderSoFar._2 &
                ("type=radio [" + name + "]") #> value

              if (name == "id")
                (Some(value), updatedBinder)
              else
                (binderSoFar._1, updatedBinder)
            case _ =>
              binderSoFar
          }
        }

      "label" #> { ns: NodeSeq => ns match {
        case label: Elem if label.label == "label" =>
          val nonTextChildren = label.child.filterNot(_.isInstanceOf[Text])

          val updatedLabel =
            idAttribute.foldLeft(label.copy(child = Text(option.label) ++ nonTextChildren))(_ % ("for", _))

          radioBinderWithAttributes apply updatedLabel
      } } &
      radioBinderWithAttributes
    }
  }

  // We're replacing the whole select element, which means that we need to
  // apply the validation/binder conversions to the resulting element directly.
  def selectBinder = {
    def selected(in: Boolean) = {
      import  scala.xml._

      if (in)
        new UnprefixedAttribute("selected", "selected", Null)
      else
        Null
    }

    val functionId = fieldName.is
    val select =
      <select name={functionId}>{
        noncedOptions.map { option =>
          option.attrs.foldLeft(<option value={option.value}>{option.label}</option>)(_ % _) %
            selected(defaultNonce === option.value)
        }
      }</select>

    val withValidations = validations.foldLeft("nothing" #> PassThru)(_ & _.binder(selector))
    val fullBinder = eventHandlers.foldLeft(withValidations)(_ & _.binder(selector, optionProcessor))

    selector #> fullBinder(select)
  }

  protected def generateFunctionIdAndHandler: String = {
    // Awkward super-dirty, but we need to reference the function id in
    // the handler, and we only get the function id after mapping the
    // handler, so we have to go this route. As long as you don't run
    // the handler before the fmapFunc runs, everything is peachy.
    //
    // WARNING: DON'T RUN THE HANDLER BEFORE THE FMAPFUNC RUNS.
    //
    // Kword? ;)
    var functionId: String = null

    def handler(incomingValue: String): Unit = {
      convertValue(incomingValue) match {
        case Full(convertedValue) =>
          val validationErrors = validations.reverse.flatMap(_(convertedValue))

          addValidationErrors(validationErrors: _*)

          if (validationErrors.isEmpty)
            fieldValue(Full(convertedValue))
          else
            fieldValue(Failure(convertedValue + " failed validations.") ~> validationErrors)

        case failure @ Failure(failureError, _, _) =>
          fieldValue(failure ~> incomingValue)
          addValidationErrors(failureError)
        case Empty =>
          fieldValue(Failure("Unrecognized response.") ~> incomingValue)
          addValidationErrors("Unrecognized response.")
      }
    }

    functionId = S.fmapFunc(handler _)(funcName => funcName)

    functionId
  }
}

/**
 * This case class creates a field holder for a multi select field that gets a
 * list of `Strings` from the client. If `asCheckboxes` is `true`, the
 * binder created for this select field is designed to bind to checkboxes
 * and their labels instead of producing a select field.
 *
 * When dealing with `select` elements (i.e., `asCheckboxes` is `false`),
 * the whole element specified by `selector` will be replaced by a new `select`
 * element.
 *
 * When `asCheckboxes` is `true`, the element specified by `selector` will be
 * repeated once for each of the passed `options`. The option label will be
 * used to set the text of the `label`. If the checkbox is nested in the
 * `label`, it will be left at the end of the `label`. If a given
 * `SelectableOption` specifies an `id` attribute, in addition to that
 * attribute being set on the checkbox, the `for` attribute of the `label`
 * element (if present) will be set to the same value.
 */
case class MultiSelectFieldHolder[
  FieldValueType,
  ValidationType >: FieldValueType,
  EventHandlerType >: FieldValueType
](
  selector: String,
  initialValues: List[FieldValueType],
  options: List[SelectableOption[FieldValueType]],
  validations: List[Validation[List[ValidationType]]],
  boxedValidations: List[Validation[Box[List[ValidationType]]]],
  eventHandlers: List[EventHandler[List[EventHandlerType]]],
  asCheckboxes: Boolean
) extends BaseFieldHolder[String, List[FieldValueType], List[ValidationType], List[EventHandlerType]](
            selector, Full(initialValues), validations, boxedValidations, eventHandlers
          )(
            eventHandlerValueConverter = { eventHandlingValue: String => Empty /* we don't get file values for event handlers */ }
          ) {
  val (noncedOptions, defaultNonces, optionProcessor) = {
    final case class SelectableOptionWithNonce[+T](value: T, nonce: String, label: String, attrs: ElemAttr*)

    val secure = options.map { selectableOption =>
      SelectableOptionWithNonce(selectableOption.value, randomString(20), selectableOption.label, selectableOption.attrs: _*)
    }

    val defaultNonces = secure.collect {
      case option if initialValues.contains(option.value) =>
        option.nonce
    }

    val nonces = secure.map { selectableOptionWithNonce =>
      SelectableOption(selectableOptionWithNonce.nonce, selectableOptionWithNonce.label, selectableOptionWithNonce.attrs: _*)
    }

    def process(incomingNonces: List[String]): List[FieldValueType] = {
      secure.collect {
        case SelectableOptionWithNonce(value, nonce, _, _) if incomingNonces.contains(nonce) =>
          value
      }
    }

    (nonces, defaultNonces, process _)
  }

  // FIXME hack warning. Because BaseFieldHolder currently works with
  // String instead of List[String] in its handler, it's not appropriate
  // for this. We stub this out and deal with the proper List[String] in
  // a customized way. We'll want to fix BaseFieldHolder to accommodate
  // both String and List[String] instead, with a default convertListValue
  // implementation that invokes convertValue, which is optional for
  // subclasses, with the head of the list or something.
  protected def convertValue(incomingValue: String) = {
    Empty
  }
  // We don't actually use this directly; instead, we set up the default value above.
  protected def serializeValue(value: List[FieldValueType]) = ""

  def validatingWith(validation: Validation[List[ValidationType]]) = {
    this.copy(validations = validation :: validations)
  }
  def validatingWith(boxedValidation: Validation[Box[List[ValidationType]]])(implicit dummy: DummyImplicit) = {
    this.copy(boxedValidations = boxedValidation :: boxedValidations)
  }
  def handlingEvent(eventHandler: EventHandler[List[EventHandlerType]]) = {
    this.copy(eventHandlers = eventHandler :: eventHandlers)
  }

  override val binder: CssSel = {
    if (asCheckboxes) {
      checkboxBinder
    } else {
      selectBinder
    }
  }

  def checkboxBinder = {
    val functionId = fieldName.is

    val withValidations = validations.foldLeft("nothing" #> PassThru)(_ & _.binder(selector))
    val validationsAndEvents = eventHandlers.foldLeft(withValidations)(_ & _.binder(selector, (s:String)=>Full(optionProcessor(List(s)))))

    selector #> noncedOptions.map { option =>
      val checkboxBinder =
        "type=checkbox [name]" #> functionId &
        "type=checkbox [value]" #> option.value &
        "type=checkbox [selected]" #> Some("selected").filter(_ => defaultNonces.contains(option.value)) &
        "type=checkbox" #> validationsAndEvents

      val (idAttribute, checkboxBinderWithAttributes) =
        option.attrs.foldLeft((None: Option[String], checkboxBinder)) { (binderSoFar, attribute) =>
          attribute match {
            case BasicElemAttr(name, value) =>
              val updatedBinder =
                binderSoFar._2 &
                ("type=checkbox [" + name + "]") #> value

              if (name == "id")
                (Some(value), updatedBinder)
              else
                (binderSoFar._1, updatedBinder)
            case _ =>
              binderSoFar
          }
        }

      "label" #> { ns: NodeSeq => ns match {
        case label: Elem if label.label == "label" =>
          val nonTextChildren = label.child.filterNot(_.isInstanceOf[Text])

          val updatedLabel =
            idAttribute.foldLeft(label.copy(child = Text(option.label) ++ nonTextChildren))(_ % ("for", _))

          checkboxBinderWithAttributes apply updatedLabel
      } } &
      checkboxBinderWithAttributes
    }
  }

  // We're replacing the whole select element, which means that we need to
  // apply the validation/binder conversions to the resulting element directly.
  def selectBinder = {
    def selected(in: Boolean) = {
      import  scala.xml._

      if (in)
        new UnprefixedAttribute("selected", "selected", Null)
      else
        Null
    }

    val functionId = fieldName.is
    val select =
      <select name={functionId} multiple="multiple">{
        noncedOptions.map { option =>
          option.attrs.foldLeft(<option value={option.value}>{option.label}</option>)(_ % _) %
            selected(defaultNonces.contains(option.value))
        }
      }</select>

    val withValidations = validations.foldLeft("nothing" #> PassThru)(_ & _.binder(selector))
    val fullBinder = eventHandlers.foldLeft(withValidations)(_ & _.binder(selector, (s: String)=>Full(optionProcessor(List(s)))))

    selector #> fullBinder(select)
  }

  private var convertedValues: List[FieldValueType] = Nil
  protected def generateFunctionIdAndHandler: String = {
    // Awkward super-dirty, but we need to reference the function id in
    // the handler, and we only get the function id after mapping the
    // handler, so we have to go this route. As long as you don't run
    // the handler before the fmapFunc runs, everything is peachy.
    //
    // WARNING: DON'T RUN THE HANDLER BEFORE THE FMAPFUNC RUNS.
    //
    // Kword? ;)
    var functionId: String = null

    def handler(incomingValues: List[String]): Unit = {
      optionProcessor(incomingValues) match {
        case convertedValues =>
          val validationErrors = validations.reverse.flatMap(_(convertedValues))

          addValidationErrors(validationErrors: _*)

          if (validationErrors.isEmpty)
            fieldValue(Full(convertedValues))
          else
            fieldValue(Failure(convertedValues.mkString(", ") + " failed validations.") ~> validationErrors)
      }
    }

    functionId = S.fmapFunc(handler _)(funcName => funcName)

    functionId
  }
}


case class CheckboxFieldHolder(
  selector: String,
  initialValue: Boolean,
  validations: List[Validation[Boolean]],
  boxedValidations: List[Validation[Box[Boolean]]],
  eventHandlers: List[EventHandler[Boolean]]
) extends BaseFieldHolder[String, Boolean, Boolean, Boolean](
            selector, Full(initialValue), validations, boxedValidations, eventHandlers
          )(
            eventHandlerValueConverter = (incomingValue: String) => Full(toBoolean(incomingValue))
          ) {
  def convertValue(incomingValue: String) = {
    Full(toBoolean(incomingValue))
  }
  def serializeValue(value: Boolean) = {
    if (value)
      "true"
    else
      ""
  }

  def validatingWith(validation: Validation[Boolean]) = {
    this.copy(validations = validation :: validations)
  }
  def validatingWith(boxedValidation: Validation[Box[Boolean]])(implicit dummy: DummyImplicit) = {
    this.copy(boxedValidations = boxedValidation :: boxedValidations)
  }
  def handlingEvent(eventHandler: EventHandler[Boolean]) = {
    this.copy(eventHandlers = eventHandler :: eventHandlers)
  }

  override protected def baseTransform: CssSel = {
    val functionId = fieldName.is

    selector #> { ns: NodeSeq => ns match {
      case element: Elem =>
        val checkbox = <input type="checkbox" name={functionId} value={serializeValue(initialValue)} />

        element.attributes.foldLeft(checkbox)(_ % _) ++
        <input type="hidden" name={functionId} value="false" />
    } }
  }

  protected def generateFunctionIdAndHandler: String = {
    // Awkward super-dirty, but we need to reference the function id in
    // the handler, and we only get the function id after mapping the
    // handler, so we have to go this route. As long as you don't run
    // the handler before the fmapFunc runs, everything is peachy.
    //
    // WARNING: DON'T RUN THE HANDLER BEFORE THE FMAPFUNC RUNS.
    //
    // Kword? ;)
    var functionId: String = null

    def handler(incomingValue: String): Unit = {
      convertValue(incomingValue) match {
        case Full(convertedValue) =>
          val validationErrors = validations.reverse.flatMap(_(convertedValue))

          addValidationErrors(validationErrors: _*)

          if (validationErrors.isEmpty)
            fieldValue(Full(convertedValue))
          else
            fieldValue(Failure(convertedValue + " failed validations.") ~> validationErrors)
      }
    }

    functionId = S.fmapFunc(handler _)(funcName => funcName)

    functionId
  }
}
/**
 * This case class creates a field holder for a field that gets a
 * FileParamHolder from the client. File upload fields behave this way.
 *
 * Note that FileFieldHolders do not have value serializers: they always
 * produce an empty string for their serialized value when setting a
 * value on the client. It also takes no initial value for the same
 * reason.
 */
case class FileFieldHolder[
  FieldValueType,
  ValidationType >: FieldValueType,
  EventHandlerType >: FieldValueType
](
  selector: String,
  validations: List[Validation[ValidationType]],
  boxedValidations: List[Validation[Box[ValidationType]]],
  eventHandlers: List[EventHandler[EventHandlerType]]
)(
  implicit valueConverter: (FileParamHolder)=>Box[FieldValueType]
) extends BaseFieldHolder[FileParamHolder, FieldValueType, ValidationType, EventHandlerType](
            selector, Empty, validations, boxedValidations, eventHandlers
          )(
            eventHandlerValueConverter = { eventHandlingValue: String => Empty /* we don't get file values for event handlers */ }
          ) {
  protected def convertValue(incomingValue: FileParamHolder) = valueConverter(incomingValue)
  protected def serializeValue(value: FieldValueType) = ""

  def validatingWith(validation: Validation[ValidationType]) = {
    this.copy(validations = validation :: validations)
  }
  def validatingWith(boxedValidation: Validation[Box[ValidationType]])(implicit dummy: DummyImplicit) = {
    this.copy(boxedValidations = boxedValidation :: boxedValidations)
  }
  def handlingEvent(eventHandler: EventHandler[EventHandlerType]) = {
    this.copy(eventHandlers = eventHandler :: eventHandlers)
  }

  protected def generateFunctionIdAndHandler: String = {
    // Awkward super-dirty, but we need to reference the function id in
    // the handler, and we only get the function id after mapping the
    // handler, so we have to go this route. As long as you don't run
    // the handler before the fmapFunc runs, everything is peachy.
    //
    // WARNING: DON'T RUN THE HANDLER BEFORE THE FMAPFUNC RUNS.
    //
    // Kword? ;)
    var functionId: String = null

    def handler(incomingValue: FileParamHolder): Unit = {
      valueConverter(incomingValue) match {
        case Full(convertedValue) =>
          val validationErrors = validations.reverse.flatMap(_(convertedValue))

          addValidationErrors(validationErrors: _*)

          if (validationErrors.isEmpty)
            fieldValue(Full(convertedValue))
          else
            fieldValue(Failure(convertedValue + " failed validations.") ~> validationErrors)

        case failure @ Failure(failureError, _, _) =>
          fieldValue(failure)
          addValidationErrors(failureError)
        case Empty =>
          fieldValue(Failure("Unrecognized response."))
          addValidationErrors("Unrecognized response.")
      }
    }

    val intermediaryHandler = { fileHolder: FileParamHolder =>
      if (fileHolder.length > 0)
        handler(fileHolder)
    }
    functionId = S.fmapFunc(S.BinFuncHolder(intermediaryHandler))(funcName => funcName)

    functionId
  }

  override protected def baseTransform: CssSel = {
    super.baseTransform &
    (selector + " [type]") #> "file"
  }
}

/**
 * EventHandler is a simple class that binds an event on a field to a
 * server-side function that will be invoked with the converted value of
 * the field whenever that event fires on the client. Optionally, you
 * can also specify a function to be invoked in case the value arrives
 * but cannot be converted to the target type.
 *
 * @param invalidValueHandler If Full, this specifies a function that
 * will run if the specified event triggers on the client, but the value
 * in the field on the client cannot be converted to the expected type
 * T. In this case, the provided function is invoked with ParamFailure
 * that carries the error associated with the conversion failure, and
 * whose param is the value on the client, as an unconverted String.
 */
case class EventHandler[T](
  eventName: String,
  eventHandler: (T)=>JsCmd,
  invalidValueHandler: Box[(ParamFailure[String])=>JsCmd] = Empty
) {
  def binder(baseSelector: String, valueConverter: (String)=>Box[T]) = {
    def handler(incomingValue: String) = {
      valueConverter(incomingValue) match {
        case Full(value) =>
          eventHandler(value)

        case Failure(message, exception, chain) =>
          invalidValueHandler.map(_(ParamFailure(message, exception, chain, incomingValue))) openOr Noop
        case Empty =>
          invalidValueHandler.map(_(ParamFailure("Failed to convert value.", Empty, Empty, incomingValue))) openOr Noop
      }
    }

    (baseSelector + " [on" + eventName + "]") #> onEvent(handler _)
  }
}
