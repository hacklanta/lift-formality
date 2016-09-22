package com.hacklanta
package formality

import scala.xml._

import org.specs2.mutable._

import net.liftweb.common._
import net.liftweb.http._
import net.liftweb.util._
    import Helpers._

import HListies.HList

import Formality._

class FieldGroupSpec extends Specification {
  case class MockFieldHolder[T](value: Box[T]) extends FieldHolderBase[T] {
    val binder: CssSel = "nothing" #> ClearNodes
  }

  "FieldGroups" should {
    val nameField = MockFieldHolder(Full("Stradivarius"))
    val ageField = MockFieldHolder(Full(25))

    "allow the caller to group multiple constituent fields" in {
      val fieldGroup =
        FieldGroupBase(None).withFields(nameField, ageField)

      fieldGroup.value must beLike {
        case Full(hlist: HList) =>
          hlist.length must_== 2
      }
    }

    "allow the caller to specify a converter that takes field values and produces a Box" in {
      val fieldGroup =
        FieldGroupBase(None)
          .withFields(nameField, ageField)
          .withConverter { (name: String, age: Int) =>
            Full(
              (name must_== "Stradivarius") and
              (age must_== 25)
            )
          }

      fieldGroup.value must beLike {
        case Full(matchResult) => matchResult
      }
    }

    "reflect a caller-specified converter's failure in the field group value" in {
      val failingFieldGroup =
        FieldGroupBase(None)
          .withFields(nameField, ageField)
          .withConverter { (name: String, age: Int) =>
            Failure("It's all gone wrong!")
          }

      failingFieldGroup.value must beLike {
        case Failure(message, Empty, Empty) =>
          message must_== "It's all gone wrong!"
      }
    }

    "allow the caller to specify a converter that always returns the final value" in {
      case class Person(name: String, age: Int)

      val fieldGroup =
        FieldGroupBase(None)
          .withFields(nameField, ageField)
          .as(Person)

      fieldGroup.value must beLike {
        case Full(person: Person) =>
          person.name must_== "Stradivarius"
          person.age must_== 25
      }
    }
  }
}
