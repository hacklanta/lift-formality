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
    def :+:[T](v: T): HCons[T, ListSoFar] = {
      HCons(v, hlist)
    }

    def length: Int = {
      1 + hlist.length
    }
  }

  /**
   * The last element of an HList
   */
  final class HNil extends HList {
    override def toString = "HNil"
  }

  /**
   * The HNil singleton
   */
  val HNil = new HNil()

  /**
   * The HList cons cell
   */
  final case class HCons[+H, +T <: HList](head: H, tail: T) extends HList {
    override def toString = head + " :+: " + tail
  }

  type :+:[+H, +T <: HList] = HCons[H, T]

  object :+: {
    def unapply[H, T <: HList](in: HCons[H, T]): Option[(H, T)] = Some(in.head, in.tail)
  }

}
