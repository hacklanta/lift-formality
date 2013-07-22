package com.withoutincident
package formality

import scala.xml._

import org.specs2.mutable._
import org.specs2.execute._
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

  // Returns the field id of the int field.
  def submitForm(binder: CssSel, formFieldValue: String = "5"): Option[String] = {
    val markup: NodeSeq = binder(
      <form>{
        inputWithId("int-field") ++
        (inputWithId("submit-field") % ("type" -> "submit"))
      }</form>
    )

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
        Nil

      testReq(request) { req =>
        session.runParams(req)
      }

      intFieldName
    }
  }
}

class FormSubmissionSpec extends Specification {
  "Formality forms when submitting one typed field" should {
    "properly run the success handler on conversion" in new IntFieldContext {
      var handlerRan = false
      val testForm = form withField formField formalize() onSuccess {
        case value :+: HNil =>
          handlerRan = true
      }

      submitForm(testForm.binder())

      handlerRan must_== true
    }
    "properly convert to the target type" in new IntFieldContext {
      var processedValue = -1
      val testForm = form withField formField formalize() onSuccess {
        case value :+: HNil =>
          processedValue = value
      }

      submitForm(testForm.binder())

      processedValue must_== 5
    }
    "properly fail on invalid input" in new IntFieldContext {
      var processedValue = -1
      var failedValue: Box[Int] = Empty

      val testForm = form withField formField formalize() onSuccess {
        case value :+: HNil =>
          processedValue = value
      } onFailure {
        case failure :+: HNil =>
          failedValue = failure
      }

      submitForm(testForm.binder(), formFieldValue = "bad int")

      processedValue must_== -1
      failedValue match {
        case net.liftweb.common.Failure(_, _, _) => ok
        case other => failure("Got " + other + " instead of a failure for an invalid int.")
      }
    }
    "provide input in ParamFailure if input was invalid" in new IntFieldContext {
      var failedValue: Box[Int] = Empty

      val testForm = form withField formField formalize() onFailure {
        case failure :+: HNil =>
          failedValue = failure
      }

      submitForm(testForm.binder(), formFieldValue = "bad int")

      failedValue match {
        case ParamFailure(_, _, _, fieldValue) =>
          fieldValue must_== "bad int"
        case other =>
          failure("Got " + other + " instead of a param failure with the invalid input.")
      }
    }
  }

  "Formality forms when submitting field with validations" should {
    "not run success handler if validations don't pass" in new IntFieldContext {
      var handlerRan = false

      val validatingField = formField ? { incoming: Int =>  Full("Failed.") }
      val testForm = form withField validatingField formalize() onSuccess {
        case _ =>
          handlerRan = true
      }

      submitForm(testForm.binder())

      handlerRan must_== false
    }
    "have no validation errors on successful submission" in new IntFieldContext {
      var handlerRan = false

      val validatingField = formField ? { incoming: Int => Empty }
      val testForm = form withField validatingField formalize() onSuccess {
        case _ =>
          handlerRan = true
      }

      submitForm(testForm.binder())

      handlerRan must_== true
      S.errors must haveLength(0)
    }
    "spit out a validation error if there is one" in new IntFieldContext {
      val validatingField = formField ? { incoming: Int => Full("error") }
      val testForm = form withField validatingField formalize()

      submitForm(testForm.binder())

      S.errors must haveLength(1)
    }
    "associate the validation error with the field name" in new IntFieldContext {
      val validatingField = formField ? { incoming: Int => Full("error") }
      val testForm = form withField validatingField formalize()

      val intFieldName = submitForm(testForm.binder())

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
      val testForm = form withField validatingField formalize()

      val intFieldName = submitForm(testForm.binder())

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
      var failedValue: Box[Int] = Empty

      val validatingField =
        formField ?
          { incoming: Int => Full("error") } ?
          { incoming: Int => Full("second error") } ?
          { incoming: Int => Full("other error") }
      val testForm = form withField validatingField formalize() onFailure {
        case failure :+: HNil =>
          failedValue = failure
      }

      val intFieldName = submitForm(testForm.binder())

      failedValue match {
        case ParamFailure(_, _, _, validationErrors) =>
          validationErrors must_== List("error", "second error", "other error")
        case other =>
          failure("Got " + other + " instead of a param failure with the invalid input.")
      }
    }
  }
}
