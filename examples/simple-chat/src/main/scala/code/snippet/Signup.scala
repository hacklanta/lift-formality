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
  
  def notLoggedIn_? = true

  val menu =
    Menu.i("signup") / "signup" >>
      If(
        notLoggedIn_? _,
        () => RedirectResponse("/")
      )

  val loc = menu.loc
}
class Signup {
  def form = {
    val registrationForm =
      Formality.form withField
        field[String]("#email") withField
        field[String]("#password") ajaxFormalize() onSuccess {
          case email :+: password :+: HNil =>
            User.create(User(email, password))

            net.liftweb.http.js.JsCmds.Alert("Bam!")
      }

    SHtml.makeFormsAjax andThen
    "form" #> registrationForm.binder()
  }
}

