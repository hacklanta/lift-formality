package com.hacklanta
package formality

/**
 * Two things live here: value serializers, value converters. Value
 * serializers are T=>String functions, value converters are
 * String=>Box[T] functions (the round trips to/from Strings for field
 * values).
 *
 * These are all implicits so that they can be plugged into implicit
 * resolution for FieldHolders.
 */
trait FieldValueHelpers {
  import net.liftweb.common._
  import net.liftweb.util.Helpers._

  // Value serializers
  implicit val intValueSerializer = { value: Int => value.toString }

  // Value converters
  implicit val stringValueConverter = { value: String => Full(value) }
  implicit val intValueConverter = asInt _
}
