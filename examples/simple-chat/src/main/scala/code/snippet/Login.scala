package code
package snippet

import net.liftweb.http._
import net.liftweb.util.Helpers._

import model.User

import com.hacklanta.formality._
  import Formality._

object Login {
  import net.liftweb.sitemap._
    import Loc._

  val menu =
    Menu.i("login") / "login" >>
      If(
        LoginHelpers.notLoggedIn_? _,
        () => RedirectResponse("/")
      )

  val loc = menu.loc
}
class Login {
  def form = {
    val loginForm =
      Formality.form withFields(
        fieldGroup.withFields(
          field[String]("#email"),
          field[String]("#password")
        ) withConverter(User.findUser _)
      ) onSuccess { user =>
        LoginHelpers.logUserIn(user)

        S.redirectTo("/")
      }

      "form" #> loginForm.binder
  }
}
