package com.hacklanta
package formality

import shapeless._

class FormUnapplier[
  ApplyType, UnapplyParameterType,
  CaseClassType <: Product,
  ParameterHList <: HList
](
  apply: ApplyType,
  unapply: UnapplyParameterType => Option[CaseClassType]
)(
  implicit fnHlister: FnHListerAux[ApplyType, ParameterHList => UnapplyParameterType],
           hlister: HListerAux[CaseClassType, ParameterHList]
) {
  val iso = Iso.hlist(apply, unapply)

  def unapply(hlist: ParameterHList): Option[UnapplyParameterType] = {
    Some(iso.from(hlist))
  }
}
