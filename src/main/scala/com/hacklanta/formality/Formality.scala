package com.hacklanta
package formality

import net.liftweb.http.FileParamHolder
import net.liftweb.http.js.JsCmd
import net.liftweb.http.SHtml.SelectableOption
import net.liftweb.common._
import net.liftweb.util._
  import Helpers._

import shapeless._

object Formality extends FieldValueHelpers {
  def field[T](selector: String, initialValue: T)(implicit valueConverter: (String)=>Box[T], valueSerializer: (T)=>String): SimpleFieldHolder[T, T, T] = {
    SimpleFieldHolder(selector, Full(initialValue), Nil, Nil)(valueConverter, valueSerializer)
  }
  def field[T](selector: String)(implicit valueConverter: (String)=>Box[T], valueSerializer: (T)=>String): SimpleFieldHolder[T, T, T] = {
    SimpleFieldHolder[T,T,T](selector, Empty, Nil, Nil)(valueConverter, valueSerializer)
  }

  // Basic file upload field, spits out a FileParamHolder.
  def fileUploadField(selector: String): FileFieldHolder[FileParamHolder, FileParamHolder, FileParamHolder] = {
    FileFieldHolder(selector, Nil, Nil)(fph => Full(fph))
  }
  def typedFileUploadField[T](selector: String)(implicit valueConverter: (FileParamHolder)=>Box[T]): FileFieldHolder[T,T,T] = {
    FileFieldHolder(selector, Nil, Nil)(valueConverter)
  }

  // Basic  select fields.
  def selectField[T](selector: String, values: List[SelectableOption[T]], asRadioButtons: Boolean = false) = {
    SelectFieldHolder[T,T,T](selector, Empty, values, Nil, Nil, asRadioButtons)
  }
  def selectField[T](selector: String, values: List[SelectableOption[T]], default: Box[T]) = {
    SelectFieldHolder[T,T,T](selector, default, values, Nil, Nil, false)
  }
  def selectField[T](selector: String, values: List[SelectableOption[T]], default: Box[T], asRadioButtons: Boolean) = {
    SelectFieldHolder[T,T,T](selector, default, values, Nil, Nil, asRadioButtons)
  }
  def selectField[T](selector: String, values: List[(T, String)])(implicit dummy: DummyImplicit) = {
    SelectFieldHolder[T,T,T](selector, Empty, values.map(value => SelectableOption(value._1, value._2)), Nil, Nil, false)
  }
  def selectField[T](selector: String, values: List[(T, String)], default: Box[T])(implicit dummy: DummyImplicit) = {
    SelectFieldHolder[T,T,T](selector, default, values.map(value => SelectableOption(value._1, value._2)), Nil, Nil, false)
  }
  def selectField[T](selector: String, values: List[T])(implicit valueSerializer: (T)=>String) = {
    SelectFieldHolder[T,T,T](selector, Empty, values.map(value => SelectableOption(value, valueSerializer(value))), Nil, Nil, false)
  }
  def selectField[T](selector: String, values: List[T], default: Box[T])(implicit valueSerializer: (T)=>String) = {
    SelectFieldHolder[T,T,T](selector, default, values.map(value => SelectableOption(value, valueSerializer(value))), Nil, Nil, false)
  }

  def checkboxField(selector: String, default: Boolean = false) = {
    CheckboxFieldHolder(selector, default, Nil, Nil)
  }

  def on[T](eventName: String, handler: (T)=>JsCmd) = EventHandler[T](eventName, handler)

  def form = FormalityFormProto[HNil, HNil, HNil](HNil)

  val :+: = shapeless.::
  val HNil = shapeless.HNil
}
