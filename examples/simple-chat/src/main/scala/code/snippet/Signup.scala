package code
package snippet

import net.liftweb.common._
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
      Formality.form.withFields(
        fieldGroup.withFields(
          emailField,
          passwordField
        ).as(User.apply _)
      ) onSuccess { user =>
        LoginHelpers.logUserIn(User.create(user))

        S.redirectTo("/")
      }

    "form" #> registrationForm.binder
  }
}

