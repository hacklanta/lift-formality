package com.withoutincident
package formality

import net.liftweb.http.js.JsCmd
import net.liftweb.common._
import net.liftweb.util._
  import Helpers._

import shapeless._

object Formality {
  def field[T](selector: String, initialValue: T)(implicit valueConverter: (String)=>Box[T], valueSerializer: (T)=>String): FieldHolder[T, T, T, T] = {
    FieldHolder(selector, Full(initialValue), Nil, Nil)(valueConverter, valueSerializer)
  }
  def field[T](selector: String)(implicit valueConverter: (String)=>Box[T], valueSerializer: (T)=>String): FieldHolder[T, T, T, T] = {
    field[T](selector)(valueConverter, valueSerializer)
  }

  def on[T](eventName: String, handler: (T)=>JsCmd) = EventHandler[T](eventName, handler)

  def form = FormalityFormProto[HNil, HNil, HNil](HNil)
}
