package com.hacklanta
package formality

import net.liftweb.common.Empty
import net.liftweb.http.{LiftSession,S}
import net.liftweb.util.StringHelpers

import org.specs2.execute.AsResult
import org.specs2.mutable.Around
import org.specs2.specification.Scope

trait SessionContext {
  val session = new LiftSession("", StringHelpers.randomString(20), Empty)
}
trait SScope extends Around with SessionContext with Scope {
  def around[T : AsResult](t: =>T) = {
    S.initIfUninitted(session) {
      AsResult(t)  // execute t inside a http session
    }
  }
}

