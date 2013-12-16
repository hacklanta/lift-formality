package code
package model

case class ChatMessage(poster: String, body: String)

object ChatMessage {
  @volatile var messages = List[ChatMessage]()

  def create(message: ChatMessage) = {
    messages ::= message

    message
  }
}
