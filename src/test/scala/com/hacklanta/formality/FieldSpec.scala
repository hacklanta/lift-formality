package com.hacklanta
package formality

import scala.xml._

import org.specs2.mutable._
import org.specs2.execute._

import net.liftweb.common._
import net.liftweb.http._
import net.liftweb.util._

import Formality._

trait SessionContext {
  val session = new LiftSession("", StringHelpers.randomString(20), Empty)
}
trait SContext extends Around with SessionContext {
  def around[T <% Result](t: =>T) = {
    S.initIfUninitted(session) {
      AsResult(t)  // execute t inside a http session
    }
  }
}

class FieldSpec extends Specification {
  val templateElement = <div class="boomdayada boomdayadan" data-test-attribute="bam">Here's a test!</div>

  "Simple fields with no initial value" should {
    "only bind the name attribute" in new SContext {
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
  
  "Simple fields with an initial value" should {
    "only bind the name and value attributes" in new SContext {
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

  "Regular file upload fields" should {
    "only bind the name and type attributes" in new SContext {
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

  "Typed file upload fields" should {
    "only bind the name and type attributes" in new SContext {
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

  "Select object fields with SelectableOptions" should {
    val objects = List(
      SHtml.SelectableOption(new Exception("ohai"), "ohai"),
      SHtml.SelectableOption(new Exception("obai"), "obai"),
      SHtml.SelectableOption(new Exception("slabai"), "slabai")
    )

    "replace the element wholesale with a select element" in new SContext {
      val formField = selectField[Exception](".boomdayada", objects)

      val resultingMarkup = <test-parent>{formField.binder(templateElement)}</test-parent>

      resultingMarkup must \(
        "select",
        "class" -> "boomdayada boomdayadan",
        "data-test-attribute" -> "bam",
        "name" -> ".*"
      )
    }
    "carry any SelectableOption attributes into the resulting options" in new SContext {
      val objects = List(
        SHtml.SelectableOption(new Exception("ohai"), "ohai", ("test" -> "bam")),
        SHtml.SelectableOption(new Exception("obai"), "obai", ("other-test" -> "bam")),
        SHtml.SelectableOption(new Exception("slabai"), "slabai", ("still-other-test" -> "bam"))
      )

      val formField = selectField[Exception](".boomdayada", objects)

      val resultingMarkup = <test-parent>{formField.binder(templateElement)}</test-parent>

      resultingMarkup must \\(
        <option>ohai</option>,
        "test" -> "bam"
      )
      resultingMarkup must \\(
        <option>obai</option>,
        "other-test" -> "bam"
      )
      resultingMarkup must \\(
        <option>slabai</option>,
        "still-other-test" -> "bam"
      )
    }
    "mark as selected the default object" in new SContext {
      val default = new Exception("ohai")
      val objects = List(
        SHtml.SelectableOption(default, "ohai"),
        SHtml.SelectableOption(new Exception("obai"), "obai"),
        SHtml.SelectableOption(new Exception("slabai"), "slabai")
      )

      val formField = selectField[Exception](".boomdayada", objects, Full(default))

      val resultingMarkup = <test-parent>{formField.binder(templateElement)}</test-parent>

      resultingMarkup must \\(
        <option>ohai</option>,
        "selected" -> "selected"
      )
    }
  }
  "Radio select object fields with SelectableOptions" should {
    val objects = List(
      SHtml.SelectableOption(new Exception("ohai"), "ohai"),
      SHtml.SelectableOption(new Exception("obai"), "obai"),
      SHtml.SelectableOption(new Exception("slabai"), "slabai")
    )

    val templateElement =
      <ul class="boomdayada boomdayadan" data-test-attribute="bam">
        <li>
          <label>
            Here's a test!
            <input type="radio" />
          </label>
        </li>
      </ul>

    "only bind to radio buttons and labels in the markup" in new SContext {
      val formField = selectField[Exception]("li", objects, asRadioButtons = true)

      val resultingMarkup = <test-parent>{formField.binder(templateElement)}</test-parent>

      resultingMarkup must not have \("select")

      (resultingMarkup \\ "label").zip(objects).foreach {
        case (label, option) =>
          label.text must_== option.label
      }

      val inputs = resultingMarkup \\ "input"

      // They should all have the same name.
      (Set[String]() ++ inputs.map(_ \ "@name").collect { case Group(Seq(Text(name))) => name }).size must_== 1
      // They should all have different values.
      (Set[String]() ++ inputs.map(_ \ "@value").collect { case Group(Seq(Text(value))) => value }).size must_== 3
    }
    "carry any SelectableOption attributes into the resulting radio buttons" in new SContext {
      val objects = List(
        SHtml.SelectableOption(new Exception("ohai"), "ohai", ("test" -> "bam")),
        SHtml.SelectableOption(new Exception("obai"), "obai", ("other-test" -> "bam")),
        SHtml.SelectableOption(new Exception("slabai"), "slabai", ("still-other-test" -> "bam"))
      )

      val formField = selectField[Exception](".boomdayada", objects, asRadioButtons = true)

      val resultingMarkup = <test-parent>{formField.binder(templateElement)}</test-parent>

      resultingMarkup must \\(
        <input type="radio" />,
        "test" -> "bam"
      )
      resultingMarkup must \\(
        <input type="radio" />,
        "other-test" -> "bam"
      )
      resultingMarkup must \\(
        <input type="radio" />,
        "still-other-test" -> "bam"
      )
    }
    "set the associated label's for attribute when an option has the id attribute set" in new SContext {
      val objects = List(
        SHtml.SelectableOption(new Exception("ohai"), "ohai", ("id" -> "bam")),
        SHtml.SelectableOption(new Exception("obai"), "obai", ("other-test" -> "bam")),
        SHtml.SelectableOption(new Exception("slabai"), "slabai", ("id" -> "boom"))
      )

      val formField = selectField[Exception](".boomdayada", objects, asRadioButtons = true)

      val resultingMarkup = <test-parent>{formField.binder(templateElement)}</test-parent>

      resultingMarkup must \\(
        "label",
        "for" -> "bam"
      ) \> "ohai"
      resultingMarkup must \\(
        "label",
        "for" -> "boom"
      ) \> "slabai"
    }
    "mark as selected the default object" in new SContext {
      val default = new Exception("ohai")
      val objects = List(
        SHtml.SelectableOption(default, "ohai"),
        SHtml.SelectableOption(new Exception("obai"), "obai"),
        SHtml.SelectableOption(new Exception("slabai"), "slabai")
      )

      val formField = selectField[Exception](".boomdayada", objects, Full(default), asRadioButtons = true)

      val resultingMarkup = <test-parent>{formField.binder(templateElement)}</test-parent>

      val selectedLabel =
        (resultingMarkup \\ "label") collect {
          case label: Elem if label.label == "label" && label.text == "ohai" =>
            label
        }

      selectedLabel must \(
        <input type="radio" />,
        "selected" -> "selected"
      )
    }
  }
  "Select object fields with tuples" should {
    val objects = List(
      (new Exception("ohai"), "ohai"),
      (new Exception("obai"), "obai"),
      (new Exception("slabai"), "slabai")
    )

    "replace the  element wholesale with a select element" in new SContext {
      val formField = selectField[Exception](".boomdayada", objects)

      val resultingMarkup = <test-parent>{formField.binder(templateElement)}</test-parent>

      resultingMarkup must \(
        "select",
        "class" -> "boomdayada boomdayadan",
        "data-test-attribute" -> "bam",
        "name" -> ".*"
      )
    }
    "mark as selected the default object" in new SContext {
      val default = new Exception("ohai")
      val objects = List(
        (default, "ohai"),
        (new Exception("obai"), "obai"),
        (new Exception("slabai"), "slabai")
      )

      val formField = selectField[Exception](".boomdayada", objects, Full(default))

      val resultingMarkup = <test-parent>{formField.binder(templateElement)}</test-parent>

      resultingMarkup must \\(
        <option>ohai</option>,
        "selected" -> "selected"
      )
    }
  }
  "Select object fields with just an object" should {
    val objects = List(
      "ohai",
      "obai",
      "slabai"
    )

    "replace the  element wholesale with a select element" in new SContext {
      val formField = selectField[String](".boomdayada", objects)

      val resultingMarkup = <test-parent>{formField.binder(templateElement)}</test-parent>

      resultingMarkup must \(
        "select",
        "class" -> "boomdayada boomdayadan",
        "data-test-attribute" -> "bam",
        "name" -> ".*"
      )
    }
    "mark as selected the default object" in new SContext {
      val formField = selectField[String](".boomdayada", objects, Full("ohai"))

      val resultingMarkup = <test-parent>{formField.binder(templateElement)}</test-parent>

      resultingMarkup must \\(
        <option>ohai</option>,
        "selected" -> "selected"
      )
    }
  }

  "Checkbox fields with Boolean values" should {
    "replace the element with a checkbox-hidden input pair" in new SContext {
      val formField = checkboxField(".boomdayada", false)

      val resultingMarkup = <test-parent>{formField.binder(templateElement)}</test-parent>

      resultingMarkup must \(
        "input",
        "type" -> "checkbox",
        "name" -> ".*"
      )
      resultingMarkup must \(
        "input",
        "type" -> "hidden",
        "name" -> ".*"
      )

      def nameForType(fieldType: String) = {
        resultingMarkup \ "input" collectFirst {
          case e: Elem if e.attribute("type") == Some(Text(fieldType)) =>
            e.attribute("name").map(_.text)
        } getOrElse {
          None
        }
      }

      nameForType("checkbox") must_== nameForType("hidden")
    }
  }
}
