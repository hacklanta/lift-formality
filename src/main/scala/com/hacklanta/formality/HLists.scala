package net.liftweb
package common

// WHOA NELLY
// Slightly redefined HLists (we want this in Lift). This allows HLists to be
// covariant in their parameters, which makes things work properly.

object HListies {

  /**
   * The trait that defines HLists
   */
  sealed trait HList

  implicit final class HListMethods[ListSoFar <: HList](hlist: ListSoFar) extends AnyRef {
    def :+:[T](v: T): :+:[T, ListSoFar] = {
      HListies.:+:(v, hlist)
    }

    def length: Int = {
      if (hlist == HNil) {
        0
      } else {
        hlist match {
          case head :+: rest =>
            1 + rest.length
        }
      }
    }
  }

  /**
   * The HNil singleton
   */
  final object HNil extends HList {
    override def toString = "HNil"
  }
  type HNil = HNil.type

  /**
   * The HList cons cell
   */
  final case class :+:[+H, +T <: HList](head: H, tail: T) extends HList {
    override def toString = head.toString + " :+: " + tail
  }

  //type :+:[+H, +T <: HList] = HCons[H, T]

  /*object :+: {
    def unapply[H, T <: HList](in: HCons[H, T]): Option[(H, T)] = Some(in.head, in.tail)
  }*/

}
