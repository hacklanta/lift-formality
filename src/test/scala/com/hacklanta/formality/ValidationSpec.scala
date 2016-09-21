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

class ValidationSpec extends Specification {
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
    "associate multiple validation errors with their field name" in new IntFieldContext {
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
    "associate validation errors with multiple field names if needed" in new IntFieldContext {
      val validatingField =
        formField ?
          { incoming: Int => Full("error") }
      val otherValidatingField =
        field[Int]("#int-field-2") ?
          { incoming: Int => Full("bam") } ?
          { incoming: Int => Full("dat fail") }
      val testForm = form.withField(validatingField).withField(otherValidatingField).formalize

      val binder =
        ("form *+" #> inputWithId("int-field-2")) andThen
        testForm.binder

      val formMarkup = bindForm(binder)
      val secondIntFieldName = inputNameForId("int-field-2", formMarkup)

      val intFieldName = submitForm(formMarkup, additionalValues = secondIntFieldName.toList.map(_ -> "8"))

      intFieldName match {
        case Some(fieldName) =>
          S.errors collect {
            case (error, Full(name)) if fieldName == name =>
              error
          } must haveLength(1)

        case _ =>
          failure("Form transformation failed.")
      }

      secondIntFieldName match {
        case Some(fieldName) =>
          S.errors collect {
            case (error, Full(name)) if fieldName == name =>
              error
          } must haveLength(2)

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
