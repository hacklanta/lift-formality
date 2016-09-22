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
    val emptyField = MockFieldHolder[String](Empty)

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

    "allow the caller to specify a converter that takes boxed field values and produces a Box" in {
      val fieldGroup =
        FieldGroupBase(None)
          .withFields(nameField, ageField, emptyField)
          .withBoxedConverter { (name: Box[String], age: Box[Int], empty: Box[String]) =>
            Full(
              (name must_== Full("Stradivarius")) and
              (age must_== Full(25)) and
              (empty must_== Empty)
           )
          }

      fieldGroup.value must beLike {
        case Full(matchResult) => matchResult
      }
    }

    "reflect a caller-specified boxed converter's failure in the field group value" in {
      val failingFieldGroup =
        FieldGroupBase(None)
          .withFields(nameField, ageField, emptyField)
          .withBoxedConverter { (name: Box[String], age: Box[Int], empty: Box[String]) =>
            Failure("It's all gone wrong!")
          }

      failingFieldGroup.value must beLike {
        case Failure(message, Empty, Empty) =>
          message must_== "It's all gone wrong!"
      }
    }
  }

  "FieldGroups with enough fields to trigger HList-only converters" should {
    val nameField = MockFieldHolder(Full("Stradivarius"))
    val ageField = MockFieldHolder(Full(25))
    val emptyField = MockFieldHolder[String](Empty)

    val fieldGroup =
      FieldGroupBase(None).withFields(
        nameField, nameField, nameField, nameField, nameField, nameField, nameField, nameField, nameField, nameField, nameField,
        ageField, ageField, ageField, ageField, ageField, ageField, ageField, ageField, ageField, ageField, ageField, ageField)
    val fieldGroupWithEmpty =
      FieldGroupBase(None).withFields(
        nameField, nameField, nameField, nameField, nameField, nameField, nameField, nameField, nameField, nameField, nameField,
        ageField, ageField, ageField, ageField, ageField, ageField, ageField, ageField, ageField, ageField, ageField,
        emptyField)

    "allow the caller to group more than 22 constituent fields" in {
      fieldGroup.value must beLike {
        case Full(hlist: HList) =>
          hlist.length must_== 23
      }
    }

    "allow the caller to specify a converter that takes an HList of field values and produces a Box" in {
      val convertibleFieldGroup =
        fieldGroup.withHlistConverter { valueHlist =>
          Full(
            (valueHlist.length must_== 23) and
            (valueHlist must beLike {
              case name1 :+: name2 :+: name3 :+: name4 :+: name5 :+: name6 :+: name7 :+: name8 :+: name9 :+: name10 :+: name11 :+:
                age1 :+: age2 :+: age3 :+: age4 :+: age5 :+: age6 :+: age7 :+: age8 :+: age9 :+: age10 :+: age11 :+: age12 :+: HNil =>
                name1 must_== "Stradivarius"
                name2 must_== "Stradivarius"
                name3 must_== "Stradivarius"
                name4 must_== "Stradivarius"
                name5 must_== "Stradivarius"
                name6 must_== "Stradivarius"
                name7 must_== "Stradivarius"
                name8 must_== "Stradivarius"
                name9 must_== "Stradivarius"
                name10 must_== "Stradivarius"
                name11 must_== "Stradivarius"
                age1 must_== 25
                age2 must_== 25
                age3 must_== 25
                age4 must_== 25
                age5 must_== 25
                age6 must_== 25
                age7 must_== 25
                age8 must_== 25
                age9 must_== 25
                age10 must_== 25
                age11 must_== 25
                age12 must_== 25
            })
          )
        }

      convertibleFieldGroup.value must beLike {
        case Full(matchResult) => matchResult
      }
    }

    "reflect a caller-specified Hlist converter's failure in the field group value" in {
      val failingFieldGroup =
        fieldGroup
          .withHlistConverter { values =>
            Failure("It's all gone wrong!")
          }

      failingFieldGroup.value must beLike {
        case Failure(message, Empty, Empty) =>
          message must_== "It's all gone wrong!"
      }
    }

    "allow the caller to specify a converter that takes an HList of boxed field values and produces a Box" in {
      val boxConvertibleFieldGroup =
        fieldGroupWithEmpty
          .withBoxedHlistConverter { boxedHlist =>
            Full(
              boxedHlist must beLike {
                case name1 :+: name2 :+: name3 :+: name4 :+: name5 :+: name6 :+: name7 :+: name8 :+: name9 :+: name10 :+: name11 :+:
                  age1 :+: age2 :+: age3 :+: age4 :+: age5 :+: age6 :+: age7 :+: age8 :+: age9 :+: age10 :+: age11 :+: empty :+: HNil =>
                  name1 must_== Full("Stradivarius")
                  name2 must_== Full("Stradivarius")
                  name3 must_== Full("Stradivarius")
                  name4 must_== Full("Stradivarius")
                  name5 must_== Full("Stradivarius")
                  name6 must_== Full("Stradivarius")
                  name7 must_== Full("Stradivarius")
                  name8 must_== Full("Stradivarius")
                  name9 must_== Full("Stradivarius")
                  name10 must_== Full("Stradivarius")
                  name11 must_== Full("Stradivarius")
                  age1 must_== Full(25)
                  age2 must_== Full(25)
                  age3 must_== Full(25)
                  age4 must_== Full(25)
                  age5 must_== Full(25)
                  age6 must_== Full(25)
                  age7 must_== Full(25)
                  age8 must_== Full(25)
                  age9 must_== Full(25)
                  age10 must_== Full(25)
                  age11 must_== Full(25)
                  empty must_== Empty
              }
            )
          }

      boxConvertibleFieldGroup.value must beLike {
        case Full(matchResult) => matchResult
      }
    }

    "reflect a caller-specified boxed HList converter's failure in the field group value" in {
      val failingFieldGroup =
        fieldGroupWithEmpty
          .withBoxedHlistConverter { boxedHlist =>
            Failure("It's all gone wrong!")
          }

      failingFieldGroup.value must beLike {
        case Failure(message, Empty, Empty) =>
          message must_== "It's all gone wrong!"
      }
    }
  }
}
