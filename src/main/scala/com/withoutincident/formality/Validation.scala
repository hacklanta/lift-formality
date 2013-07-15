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
