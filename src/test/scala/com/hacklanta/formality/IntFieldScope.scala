package com.hacklanta
package formality

import scala.xml._

import net.liftweb.http.S
import net.liftweb.mocks._
import net.liftweb.mockweb.MockWeb._
import net.liftweb.util._
  import Helpers._

import Formality._

trait IntFieldScope extends SScope {
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

    submitField.flatMap { submitFieldName =>
      val request = new MockHttpServletRequest(url = "/")
      val baseParameters: List[(String, String)] = (submitFieldName -> "_") :: additionalValues
      request.parameters = parametersForForm(intField, formFieldValue) ++ baseParameters

      testReq(request) { req =>
        session.runParams(req)
      }

      intField
    }
  }

  def parametersForForm(intFieldName: Option[String], intFieldValue: String): List[(String,String)] = {
    intFieldName.toList.map { fieldName =>
      (fieldName -> intFieldValue)
    }
  }

  // Returns the field id of the int field.
  def bindAndSubmitForm(binder: (NodeSeq)=>NodeSeq, formFieldValue: String = "5", additionalValues: List[(String,String)] = Nil): Option[String] = {
    val markup = bindForm(binder)
    submitForm(markup, formFieldValue, additionalValues)
  }
}

