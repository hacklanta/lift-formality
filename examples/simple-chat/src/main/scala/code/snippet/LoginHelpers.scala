package code
package snippet

import net.liftweb.common._
import net.liftweb.http.SessionVar

import model.User

object loggedInUser extends SessionVar[Box[User]](Empty)
object LoginHelpers {
  def notLoggedIn_? = loggedInUser.is.isEmpty

  def logUserIn(user: User) = {
    loggedInUser(Full(user))
  }
}
