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

  def submitForm(binder: CssSel, formFieldValue: String = "5"): Unit = {
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
    } {
      val request = new MockHttpServletRequest(url = "/")
      request.parameters =
        (intFieldName -> formFieldValue) ::
        (submitFieldName -> "_") ::
        Nil

      testReq(request) { req =>
        session.runParams(req)
      }
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
        case some => failure("Got " + some + " instead of a failure for an invalid int.")
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
        case some =>
          failure("Got " + some + " instead of a param failure with the invalid input.")
      }
    }
  }
}
