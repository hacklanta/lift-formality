package code
package snippet

import net.liftweb.http._
import net.liftweb.util.Helpers._

import com.hacklanta.formality.Formality
  import Formality._

import model.User

object Signup {
  import net.liftweb.sitemap._
    import Loc._

  val menu =
    Menu.i("signup") / "signup" >>
      If(
        LoginHelpers.notLoggedIn_? _,
        () => RedirectResponse("/")
      )

  val loc = menu.loc
}
class Signup {
  def form = {
    val registrationForm =
      Formality.form withField
        field[String]("#email") withField
        field[String]("#password") formalize() onSuccess {
          case email :+: password :+: HNil =>
            LoginHelpers.logUserIn(User.create(User(email, password)))

            S.redirectTo("/")
      }

    "form" #> registrationForm.binder()
  }
}

