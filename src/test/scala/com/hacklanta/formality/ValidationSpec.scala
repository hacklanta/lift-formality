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

    "associate multiple validation errors with their field name" in new IntFieldScope {
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

    "associate validation errors with multiple field names if needed" in new IntFieldScope {
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

      intFieldName must beLike {
        case Some(fieldName) =>
          S.errors collect {
            case (error, Full(name)) if fieldName == name =>
              error
          } must haveLength(1)
      }

      secondIntFieldName must beLike {
        case Some(fieldName) =>
          S.errors collect {
            case (error, Full(name)) if fieldName == name =>
              error
          } must haveLength(2)
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

    "invoke boxed validations even if there is no value for the field" in new IntFieldScope {
      override def bindForm(binder: (NodeSeq)=>NodeSeq): NodeSeq = {
        val markup: NodeSeq = binder(
          <form>{
            (inputWithId("submit-field") % ("type" -> "submit"))
          }</form>
        )

        markup
      }

      var validatorRanTimes = 0
      val validatingField = formField ? { incoming: Box[Int] => validatorRanTimes += 1; Empty }
      val testForm = form.withField(validatingField).formalize

      bindAndSubmitForm(testForm.binder)

      validatorRanTimes must_== 1
    }
  }
}
