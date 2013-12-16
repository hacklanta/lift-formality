package code
package snippet

import net.liftweb.http._
import net.liftweb.util.Helpers._

import com.hacklanta.formality._
  import Formality._
  import Html5Validations._

import model.ChatMessage

object Chat {
  import net.liftweb.sitemap._
    import Loc._

  val menu =
    Menu.i("home") / "index" >>
      If(
        LoginHelpers.loggedIn_? _,
        () => RedirectResponse("/login")
      )

  val loc = menu.loc
}
class Chat {
  val currentUser = loggedInUser.is openOrThrowException {
    "Chat page should not be accessible without valid login."
  }

  def form = {
    val messageField = field[String]("#message") ? notEmpty

    val registrationForm =
      Formality.form withField messageField ajaxFormalize() onSuccess {
        case message :+: HNil =>
          ChatMessage.create(ChatMessage(currentUser.email, message))

          js.JsCmds.Alert("Posted " + message + "!")
      }

    SHtml.makeFormsAjax andThen
    "form" #> registrationForm.binder()
  }
}
