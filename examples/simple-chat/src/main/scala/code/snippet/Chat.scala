package code
package snippet

import scala.xml.NodeSeq

import net.liftweb.common._
import net.liftweb.http._
  import js.JE.Call
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

  var chatMessageTemplate: Box[NodeSeq] = Empty
  def renderMessage(message: ChatMessage) = {
    chatMessageTemplate.map { template =>
      val transform =
        ".poster *" #> message.poster &
        ".body *" #> message.body
        
      transform apply template
    } openOr {
      NodeSeq.Empty
    }
  }

  def form = {
    val messageField = field[String]("#message") ? notEmpty

    val registrationForm =
      Formality.form withField messageField ajaxFormalize() onSuccess {
        case body :+: HNil =>
          val message = ChatMessage.create(ChatMessage(currentUser.email, body))

          Call("insertChatMessage", renderMessage(message).toString).cmd
      }

    SHtml.makeFormsAjax andThen
    "form" #> registrationForm.binder()
  }

  def chatMessages(ns: NodeSeq): NodeSeq = {
    chatMessageTemplate = Full(("li ^^" #> "ignored") apply ns)

    ns
  }
}
