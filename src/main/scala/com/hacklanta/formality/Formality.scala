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
  // NOTE: Order matters. In particular, reordering the declarations of the
  // List[SelectableOption] functions will probably break the compile. Why?
  // Your guess is as good as mine.
  def selectField[T](selector: String, values: List[SelectableOption[T]], asRadioButtons: Boolean = false): SelectFieldHolder[T,T,T] = {
    selectField[T](selector, values, Empty, asRadioButtons)
  }
  def selectField[T](selector: String, values: List[SelectableOption[T]], default: Box[T]) = {
    selectField[T](selector, values, default, false)
  }
  def selectField[T](selector: String, values: List[SelectableOption[T]], default: Box[T], asRadioButtons: Boolean) = {
    SelectFieldHolder[T,T,T](selector, default, values, Nil, Nil, asRadioButtons)
  }
  def selectField[T](selector: String, values: List[(T, String)])(implicit dummy: DummyImplicit) = {
    selectField[T](selector, values.map(value => SelectableOption(value._1, value._2)), false)
  }
  def selectField[T](selector: String, values: List[(T, String)], asRadioButtons: Boolean)(implicit dummy: DummyImplicit) = {
    selectField[T](selector, values.map(value => SelectableOption(value._1, value._2)), asRadioButtons)
  }
  def selectField[T](selector: String, values: List[(T, String)], default: Box[T])(implicit dummy: DummyImplicit) = {
    selectField[T](selector, values.map(value => SelectableOption(value._1, value._2)), default, false)
  }
  def selectField[T](selector: String, values: List[(T, String)], default: Box[T], asRadioButtons: Boolean)(implicit dummy: DummyImplicit) = {
    selectField[T](selector, values.map(value => SelectableOption(value._1, value._2)), default, asRadioButtons)
  }
  def selectField[T](selector: String, values: List[T])(implicit valueSerializer: (T)=>String) = {
    selectField[T](selector, values.map(value => SelectableOption(value, valueSerializer(value))), false)
  }
  def selectField[T](selector: String, values: List[T], asRadioButtons: Boolean)(implicit valueSerializer: (T)=>String) = {
    selectField[T](selector, values.map(value => SelectableOption(value, valueSerializer(value))), asRadioButtons)
  }
  def selectField[T](selector: String, values: List[T], default: Box[T])(implicit valueSerializer: (T)=>String) = {
    selectField[T](selector, values.map(value => SelectableOption(value, valueSerializer(value))), default, false)
  }
  def selectField[T](selector: String, values: List[T], default: Box[T], asRadioButtons: Boolean)(implicit valueSerializer: (T)=>String) = {
    selectField[T](selector, values.map(value => SelectableOption(value, valueSerializer(value))), default, asRadioButtons)
  }

  // Multi select fields.
  def multiSelectField[T](selector: String, values: List[SelectableOption[T]]): MultiSelectFieldHolder[T,T,T] = {
    multiSelectField[T](selector, values, Nil)
  }
  def multiSelectField[T](selector: String, values: List[SelectableOption[T]], defaults: List[T]) = {
    MultiSelectFieldHolder[T,T,T](selector, defaults, values, Nil, Nil, false)
  }
  def multiSelectField[T](selector: String, values: List[(T, String)])(implicit dummy: DummyImplicit) = {
    multiSelectField[T](selector, values.map(value => SelectableOption(value._1, value._2)))
  }
  def multiSelectField[T](selector: String, values: List[(T, String)], defaults: List[T])(implicit dummy: DummyImplicit) = {
    multiSelectField[T](selector, values.map(value => SelectableOption(value._1, value._2)), defaults)
  }
  def multiSelectField[T](selector: String, values: List[T])(implicit valueSerializer: (T)=>String) = {
    multiSelectField[T](selector, values.map(value => SelectableOption(value, valueSerializer(value))))
  }
  def multiSelectField[T](selector: String, values: List[T], defaults: List[T])(implicit valueSerializer: (T)=>String) = {
    multiSelectField[T](selector, values.map(value => SelectableOption(value, valueSerializer(value))), defaults)
  }
  def multiSelectField[T](selector: String, values: List[SelectableOption[T]], defaults: List[T]) = {
    MultiSelectFieldHolder[T,T,T](selector, defaults, values, Nil, Nil, false)
  }

  def checkboxField(selector: String, default: Boolean = false) = {
    CheckboxFieldHolder(selector, default, Nil, Nil)
  }

  def on[T](eventName: String, handler: (T)=>JsCmd) = EventHandler[T](eventName, handler)

  def form = FormalityFormProto[HNil, HNil, HNil](HNil)

  val :+: = shapeless.::
  val HNil = shapeless.HNil
}
