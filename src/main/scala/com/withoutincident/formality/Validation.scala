package com.withoutincident
package formality

import net.liftweb.common._
import net.liftweb.util._
  import Helpers._

/**
 * A Validation that validates a value of type T. It should return a
 * Box[String] that is Full with a validation error if there are
 * validation problems, or an empty box if there are no such problems.
 * 
 * Validations can provide a css selector that attaches
 * validation-related attributes to the input field.
 *
 * There is an implicit conversion from a (T)=>Box[String] function to
 * a Validation[T] that simply makes no changes to the relevant input
 * field
 */
trait Validation[T] extends Function1[T, Box[String]] {
  def apply(value: T): Box[String]

  /**
   * The base selector is the selector provided to the field constructor
   * This selector should be used to apply validation-related attributes
   * to that field. The default implementation leaves the field untouched.
   *
   * See inRange for a sample Validation that sets validation attributes on
   * its field.
   */
  def binder(baseSelector: String) = "nothing" #> "nothing"
}
object Validation {
  implicit def function2Validation[T](validationFn: (T)=>Box[String]) = {
    new Validation[T] { def apply(value: T) = validationFn(value) }
  }
}

/**
 * Provides basic checking that a number is within a range. This can be
 * used standalone, but it's recommended that you use it via one of its
 * subclasses that also provides client-side annotations for the range.
 *
 * The range is inclusive.
 *
 * Example:
 *   myField ? basicInRange(0, 100)
 */
class basicInRange(start: Int, end: Int) extends Validation[Int] {
  def apply(value: Int) = {
    if (value >= start && value <= end)
      Empty
    else
      Full("should be between " + start + " and " + end)
  }
}
object basicInRange {
  def apply(start: Int, end: Int) = new basicInRange(start, end)
}
/**
 * Provides basic checking that a string is not empty. This can be used
 * standalone, but it's recommended that you use it via one of its
 * subclasses that also provides client-side annotations for the range.
 *
 * Example:
 *   myField ? basicNotEmpty
 */
class basicNotEmpty extends Validation[String] {
  def apply(value: String) = {
    if (value.isEmpty)
      Full("should not be empty")
    else
      Empty
  }
}
object basicNotEmpty {
  def apply = new basicNotEmpty
}

trait Html5Validations {
  /**
   * Checks that a given int value is within the range [start, end]
   * (inclusive).
   */
  case class inRange(start: Int, end: Int) extends basicInRange(start, end) {
    override def binder(baseSelector: String) = {
      (baseSelector + " [min]") #> start &
      (baseSelector + " [max]") #> end
    }
  }

  /**
   * Checks that a given int value is within the range [start, end]
   * (inclusive).
   */
  case object notEmpty extends basicNotEmpty {
    override def binder(baseSelector: String) = {
      (baseSelector + " [required]") #> "required"
    }
  }
}
/**
 * Contains validations that apply HTML5 attributes to support them on
 * the client.
 */
object Html5Validations extends Html5Validations
/**
 * Contains validations that apply parsley.js attributes to support them
 * on the client. Validation names follow parsley.js names.
 *
 * FIXME These are incomplete.
 */
object ParsleyValidations extends Html5Validations {
  case object notBlank extends basicNotEmpty {
    override def binder(baseSelector: String) = {
      (baseSelector + " [data-notblank]") #> "true"
    }
  }

  case class minLength(minimumLength: Int) extends Validation[String] {
    def apply(value: String) = {
      if (value.length < minimumLength)
        Full("should be at least " + minimumLength + " characters long")
      else
        Empty
    }

    override def binder(baseSelector: String) = {
      (baseSelector + " [data-minlength]") #> minimumLength
    }
  }

  case class maxLength(maximumLength: Int) extends Validation[String] {
    def apply(value: String) = {
      if (value.length > maximumLength)
        Full("should be at least " + maximumLength + " characters long")
      else
        Empty
    }

    override def binder(baseSelector: String) = {
      (baseSelector + " [data-minlength]") #> maximumLength
    }
  }

  case class rangeLength(minimumLength: Int, maximumLength: Int) extends Validation[String] {
    def apply(value: String) = {
      if (value.length > maximumLength || value.length < minimumLength)
        Full("should be between " + minimumLength + " and " + maximumLength + " characters long")
      else
        Empty
    }

    override def binder(baseSelector: String) = {
      (baseSelector + " [data-rangelength]") #> "[%s,%s]".format(minimumLength, maximumLength)
    }
  }
}
