package com.hacklanta
package formality

import scala.xml._

import org.specs2.mutable._
import org.specs2.specification.Scope

import net.liftweb.common._
import net.liftweb.http._
import net.liftweb.mocks._
import net.liftweb.mockweb.MockWeb._
import net.liftweb.util._
  import Helpers._

import Formality._

trait IntFieldContext extends SContext with Scope {
  val templateElement = <input />

  def inputWithId(id: String) = templateElement % ("id" -> id)
  def inputNameForId(id: String, form: NodeSeq) = {
    form \ "input" collectFirst {
      case e: Elem if e.attribute("id") == Some(Text(id)) =>
        e.attribute("name").map(_.text)
    } getOrElse {
      None
    }
  }

  val formField = field[Int]("#int-field")

  def bindForm(binder: (NodeSeq)=>NodeSeq): NodeSeq = {
    val markup: NodeSeq = binder(
      <form>{
        inputWithId("int-field") ++
        (inputWithId("submit-field") % ("type" -> "submit"))
      }</form>
    )

    markup
  }

  def submitForm(markup: NodeSeq, formFieldValue: String = "5", additionalValues: List[(String,String)] = Nil): Option[String] = {
    // Lift automatically does this when wrapping up the page
    // render. Without it, our fields won't register on submission.
    session.updateFunctionMap(S.functionMap, uniqueId = "render-version", when = 0l)

    val intField = inputNameForId("int-field", markup)
    val submitField = inputNameForId("submit-field", markup)

    for {
      intFieldName <- intField
      submitFieldName <- submitField
    } yield {
      val request = new MockHttpServletRequest(url = "/")
      request.parameters =
        (intFieldName -> formFieldValue) ::
        (submitFieldName -> "_") ::
        additionalValues

      testReq(request) { req =>
        session.runParams(req)
      }

      intFieldName
    }
  }

  // Returns the field id of the int field.
  def bindAndSubmitForm(binder: (NodeSeq)=>NodeSeq, formFieldValue: String = "5", additionalValues: List[(String,String)] = Nil): Option[String] = {
    val markup = bindForm(binder)
    submitForm(markup, formFieldValue, additionalValues)
  }
}

class FormSubmissionSpec extends Specification {
  "Formality forms when submitting one typed field" should {
    "properly run the success handler on conversion" in new IntFieldContext {
      var handlerRan = false
      val testForm = form.withField(formField).formalize onSuccess { value =>
        handlerRan = true
      }

      bindAndSubmitForm(testForm.binder)

      handlerRan must_== true
    }
    "properly convert to the target type" in new IntFieldContext {
      var processedValue = -1
      val testForm = form.withField(formField).formalize onSuccess { value =>
        processedValue = value
      }

      bindAndSubmitForm(testForm.binder)

      processedValue must_== 5
    }
    "properly fail on invalid input" in new IntFieldContext {
      var processedValue = -1
      var failedValues: List[Failure] = Nil

      val testForm = form.withField(formField).formalize onSuccess { value =>
        processedValue = value
      } onFailure { failures =>
        failedValues = failures
      }

      bindAndSubmitForm(testForm.binder, formFieldValue = "bad int")

      processedValue must_== -1
      failedValues match {
        case List(net.liftweb.common.Failure(_, _, _)) => ok
        case other => failure("Got " + other + " instead of one failure for an invalid int.")
      }
    }
    "provide input in ParamFailure if input was invalid" in new IntFieldContext {
      var failedValues: List[Failure] = Nil

      val testForm = form.withField(formField).formalize onFailure { failures =>
        failedValues = failures
      }

      bindAndSubmitForm(testForm.binder, formFieldValue = "bad int")

      failedValues match {
        case List(ParamFailure(_, _, _, fieldValue)) =>
          fieldValue must_== "bad int"
        case other =>
          failure("Got " + other + " instead of one param failure with the invalid input.")
      }
    }
  }

  "Formality forms when submitting field with validations" should {
    "not run success handler if validations don't pass" in new IntFieldContext {
      var handlerRan = false

      val validatingField = formField ? { incoming: Int =>  Full("Failed.") }
      val testForm = form.withField(validatingField).formalize onSuccess { _ =>
        handlerRan = true
      }

      bindAndSubmitForm(testForm.binder)

      handlerRan must_== false
    }
    "have no validation errors on successful submission" in new IntFieldContext {
      var handlerRan = false

      val validatingField = formField ? { incoming: Int => Empty }
      val testForm = form.withField(validatingField).formalize onSuccess { _ =>
        handlerRan = true
      }

      bindAndSubmitForm(testForm.binder)

      handlerRan must_== true
      S.errors must haveLength(0)
    }
    "spit out a validation error if there is one" in new IntFieldContext {
      val validatingField = formField ? { incoming: Int => Full("error") }
      val testForm = form.withField(validatingField).formalize

      bindAndSubmitForm(testForm.binder)

      S.errors must haveLength(1)
    }
    "associate the validation error with the field name" in new IntFieldContext {
      val validatingField = formField ? { incoming: Int => Full("error") }
      val testForm = form.withField(validatingField).formalize

      val intFieldName = bindAndSubmitForm(testForm.binder)

      intFieldName match {
        case Some(fieldName) =>
          S.errors collect {
            case (error, Full(name)) if fieldName == name =>
              error
          } must haveLength(1)

        case _ =>
          failure("Form transformation failed.")
      }
    }
    "associate multiple validation errors with the field names" in new IntFieldContext {
      val validatingField =
        formField ?
          { incoming: Int => Full("error") } ?
          { incoming: Int => Full("second error") } ?
          { incoming: Int => Full("other error") }
      val testForm = form.withField(validatingField).formalize

      val intFieldName = bindAndSubmitForm(testForm.binder)

      intFieldName match {
        case Some(fieldName) =>
          S.errors collect {
            case (error, Full(name)) if fieldName == name =>
              error
          } must haveLength(3)

        case _ =>
          failure("Form transformation failed.")
      }
    }
    "provide the validation errors in the failure handler as a ParamFailure param" in new IntFieldContext {
      var failedValues: List[Failure] = Nil

      val validatingField =
        formField ?
          { incoming: Int => Full("error") } ?
          { incoming: Int => Full("second error") } ?
          { incoming: Int => Full("other error") }
      val testForm = form.withField(validatingField).formalize onFailure { failures =>
        failedValues = failures
      }

      val intFieldName = bindAndSubmitForm(testForm.binder)

      failedValues match {
        case List(ParamFailure(_, _, _, validationErrors)) =>
          validationErrors must_== List("error", "second error", "other error")
        case other =>
          failure("Got " + other + " instead of one param failure with the invalid input.")
      }
    }
  }
}
