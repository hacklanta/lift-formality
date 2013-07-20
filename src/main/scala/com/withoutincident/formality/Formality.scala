package com.withoutincident
package formality

import net.liftweb.http.FileParamHolder
import net.liftweb.http.js.JsCmd
import net.liftweb.common._
import net.liftweb.util._
  import Helpers._

import shapeless._

object Formality extends FieldValueHelpers {
  def field[T](selector: String, initialValue: T)(implicit valueConverter: (String)=>Box[T], valueSerializer: (T)=>String): SimpleFieldHolder[T, T, T, T] = {
    SimpleFieldHolder(selector, Full(initialValue), Nil, Nil)(valueConverter, valueSerializer)
  }
  def field[T](selector: String)(implicit valueConverter: (String)=>Box[T], valueSerializer: (T)=>String): SimpleFieldHolder[T, T, T, T] = {
    SimpleFieldHolder[T,T,T,T](selector, Empty, Nil, Nil)(valueConverter, valueSerializer)
  }

  // Basic file upload field, spits out a FileParamHolder.
  def fileUploadField(selector: String): FileFieldHolder[FileParamHolder, FileParamHolder, FileParamHolder] = {
    FileFieldHolder(selector, Nil, Nil)(fph => Full(fph))
  }
  def typedFileUploadField[T](selector: String)(implicit valueConverter: (FileParamHolder)=>Box[T]): FileFieldHolder[T,T,T] = {
    FileFieldHolder(selector, Nil, Nil)(valueConverter)
  }

  def on[T](eventName: String, handler: (T)=>JsCmd) = EventHandler[T](eventName, handler)

  def form = FormalityFormProto[HNil, HNil, HNil](HNil)

  val :+: = shapeless.::
  val HNil = shapeless.HNil
}
