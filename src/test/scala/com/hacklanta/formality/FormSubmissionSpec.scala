package com.hacklanta
package formality

import org.specs2.mutable._
import org.specs2.specification.Scope

import net.liftweb.common._
import net.liftweb.http._

import Formality._

class FormSubmissionSpec extends Specification {
  "Formality forms when submitting one typed field" should {
    "properly run the success handler on conversion" in new IntFieldScope {
      var handlerRan = false
      val testForm = form.withField(formField).formalize onSuccess { value =>
        handlerRan = true
      }

      bindAndSubmitForm(testForm.binder)

      handlerRan must_== true
    }
    "properly convert to the target type" in new IntFieldScope {
      var processedValue = -1
      val testForm = form.withField(formField).formalize onSuccess { value =>
        processedValue = value
      }

      bindAndSubmitForm(testForm.binder)

      processedValue must_== 5
    }
    "properly fail on invalid input" in new IntFieldScope {
      var processedValue = -1
      var failedValues: List[Failure] = Nil

      val testForm = form.withField(formField).formalize onSuccess { value =>
        processedValue = value
      } onFailure { failures =>
        failedValues = failures
      }

      bindAndSubmitForm(testForm.binder, formFieldValue = "bad int")

      processedValue must_== -1
      failedValues must beLike {
        case List(net.liftweb.common.Failure(_, _, _)) => ok
      }
    }
    "provide input in ParamFailure if input was invalid" in new IntFieldScope {
      var failedValues: List[Failure] = Nil

      val testForm = form.withField(formField).formalize onFailure { failures =>
        failedValues = failures
      }

      bindAndSubmitForm(testForm.binder, formFieldValue = "bad int")

      failedValues must beLike {
        case List(ParamFailure(_, _, _, fieldValue)) =>
          fieldValue must_== "bad int"
      }
    }
  }

  "Formality forms when submitting field with validations" should {
    "not run success handler if validations don't pass" in new IntFieldScope {
      var handlerRan = false

      val validatingField = formField ? { incoming: Int =>  Full("Failed.") }
      val testForm = form.withField(validatingField).formalize onSuccess { _ =>
        handlerRan = true
      }

      bindAndSubmitForm(testForm.binder)

      handlerRan must_== false
    }
    "have no validation errors on successful submission" in new IntFieldScope {
      var handlerRan = false

      val validatingField = formField ? { incoming: Int => Empty }
      val testForm = form.withField(validatingField).formalize onSuccess { _ =>
        handlerRan = true
      }

      bindAndSubmitForm(testForm.binder)

      handlerRan must_== true
      S.errors must haveLength(0)
    }
    "spit out a validation error if there is one" in new IntFieldScope {
      val validatingField = formField ? { incoming: Int => Full("error") }
      val testForm = form.withField(validatingField).formalize

      bindAndSubmitForm(testForm.binder)

      S.errors must haveLength(1)
    }
    "associate the validation error with the field name" in new IntFieldScope {
      val validatingField = formField ? { incoming: Int => Full("error") }
      val testForm = form.withField(validatingField).formalize

      val intFieldName = bindAndSubmitForm(testForm.binder)

      intFieldName must beLike {
        case Some(fieldName) =>
          S.errors collect {
            case (error, Full(name)) if fieldName == name =>
              error
          } must haveLength(1)
      }
    }
    "associate multiple validation errors with the field names" in new IntFieldScope {
      val validatingField =
        formField ?
          { incoming: Int => Full("error") } ?
          { incoming: Int => Full("second error") } ?
          { incoming: Int => Full("other error") }
      val testForm = form.withField(validatingField).formalize

      val intFieldName = bindAndSubmitForm(testForm.binder)

      intFieldName must beLike {
        case Some(fieldName) =>
          S.errors collect {
            case (error, Full(name)) if fieldName == name =>
              error
          } must haveLength(3)
      }
    }
    "provide the validation errors in the failure handler as a ParamFailure param" in new IntFieldScope {
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

      failedValues must beLike {
        case List(ParamFailure(_, _, _, validationErrors)) =>
          validationErrors must_== List("error", "second error", "other error")
      }
    }

    "call onSubmission handlers whether or not the form fails" in new IntFieldScope {
      var didRunFailed = false
      var didRunSuccess = false

      val validatingField =
        formField ?
          { incoming: Int => Full("error") }
      val testFailedForm =
        form.withField(validatingField).formalize onSubmission { (field: Box[Int]) =>
          didRunFailed = true
        }
      val testSuccessForm =
        form.withField(formField).formalize onSubmission { (field: Box[Int]) =>
          didRunSuccess = true
        }

      bindAndSubmitForm(testFailedForm.binder)
      bindAndSubmitForm(testSuccessForm.binder)

      didRunFailed must_== true
      didRunSuccess must_== true
    }

    "call onSubmission handlers even for AJAX forms" in new IntFieldScope {
      var didRunFailed = false
      var didRunSuccess = false

      val validatingField =
        formField ?
          { incoming: Int => Full("error") }
      val testFailedForm =
        form.withField(validatingField).ajaxFormalize onSubmission { (field: Box[Int]) =>
          didRunFailed = true
        }
      val testSuccessForm =
        form.withField(formField).ajaxFormalize onSubmission { (field: Box[Int]) =>
          didRunSuccess = true
        }

      bindAndSubmitForm(testFailedForm.binder)
      bindAndSubmitForm(testSuccessForm.binder)

      didRunFailed must_== true
      didRunSuccess must_== true
    }
  }
}
