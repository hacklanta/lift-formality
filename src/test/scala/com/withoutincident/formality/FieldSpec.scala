package com.withoutincident
package formality

import scala.xml.Elem

import org.specs2.mutable._
import org.specs2.execute._

import net.liftweb.common._
import net.liftweb.http._
import net.liftweb.util._

import Formality._

object withSContext extends Around {
  def around[T <% Result](t: =>T) = {
    val session = new LiftSession("", StringHelpers.randomString(20), Empty)
    S.initIfUninitted(session) {
      AsResult(t)  // execute t inside a http session
    }
  }
}

class FieldSpec extends Specification {
  val templateElement = <div class="boomdayada boomdayadan" data-test-attribute="bam">Here's a test!</div>

  "Formality when creating simple fields with no initial value" should {
    "only bind the name attribute" in withSContext {
      val formField = field[String](".boomdayada")

      val resultingMarkup = <test-parent>{formField.binder(templateElement)}</test-parent>

      resultingMarkup must \(
        "div",
        "class" -> "boomdayada boomdayadan",
        "data-test-attribute" -> "bam",
        "name" -> ".*"
      )
      (resultingMarkup \ "div" \ "@value").text must_== ""
    }
  }
  
  "Formality when creating simple fields with an initial value" should {
    "only bind the name and value attributes" in withSContext {
      val formField = field[String](".boomdayada", "Dat value")

      val resultingMarkup = <test-parent>{formField.binder(templateElement)}</test-parent>

      resultingMarkup must \(
        "div",
        "class" -> "boomdayada boomdayadan",
        "data-test-attribute" -> "bam",
        "name" -> ".*",
        "value" -> "Dat value"
      )
    }
  }

  "Formality when creating regular file upload fields" should {
    "only bind the name and type attributes" in withSContext {
      val formField = fileUploadField(".boomdayada")

      val resultingMarkup = <test-parent>{formField.binder(templateElement)}</test-parent>

      resultingMarkup must \(
        "div",
        "class" -> "boomdayada boomdayadan",
        "data-test-attribute" -> "bam",
        "name" -> ".*",
        "type" -> "file"
      )
    }
  }

  "Formality when creating typed file upload fields" should {
    "only bind the name and type attributes" in withSContext {
      implicit def fileToObject(fph: FileParamHolder) = Full("boom")

      val formField = typedFileUploadField[String](".boomdayada")

      val resultingMarkup = <test-parent>{formField.binder(templateElement)}</test-parent>

      resultingMarkup must \(
        "div",
        "class" -> "boomdayada boomdayadan",
        "data-test-attribute" -> "bam",
        "name" -> ".*",
        "type" -> "file"
      )
    }
  }
}
