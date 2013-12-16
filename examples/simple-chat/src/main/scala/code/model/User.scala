package code
package model

case class User(email: String, password: String)

object User {
  @volatile var users = List[User]()

  def create(user: User) = {
    users ::= user

    user
  }

  def findUser(email: String, password: String) = {
    users.collectFirst {
      case user @ User(`email`, `password`) =>
        user
    }
  }
}
