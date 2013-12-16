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
      Formality.form withField
        field[String]("#email") withField
        field[String]("#password") formalize() onSuccess {
          case email :+: password :+: HNil =>
            User.findUser(email, password).foreach { user =>
              LoginHelpers.logUserIn(user)
            }

            S.redirectTo("/")
        }

      "form" #> loginForm.binder()
  }
}