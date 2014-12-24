package code
package snippet

import net.liftweb.http._
import net.liftweb.util.Helpers._

import com.hacklanta.formality._
  import Formality._
  import Html5Validations._

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
    val emailField = field[String]("#email") ? notEmpty
    val passwordField = field[String]("#password") ? notEmpty

    val registrationForm =
      Formality.form.withFields(emailField, passwordField) onSuccess {
        case email :+: password :+: HNil =>
          LoginHelpers.logUserIn(User.create(User(email, password)))

          S.redirectTo("/")
      }

    "form" #> registrationForm.binder()
  }
}

