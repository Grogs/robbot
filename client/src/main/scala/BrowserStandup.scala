import cats.{Id, ~>}
import language.{Ask, InteractionDsl, Receive, Say}

import scala.scalajs.js.Dynamic.global
import scala.scalajs.js.JSApp
import scala.scalajs.js.annotation.JSExport

object BrowserStandup extends JSApp {
  val interpreter = new StandupInterpreter(BrowserInteractionInterpreter, null)

  @JSExport
  override def main(): Unit =
    new Standup().theStandup.foldMap(interpreter)

  object BrowserInteractionInterpreter extends (InteractionDsl ~> Id) {

    def prompt(text: String = "") = {
      println(text)
      global.prompt(text).asInstanceOf[String]
    }

    def alert(text: String) = {
      global.alert(text).asInstanceOf[Unit]
    }

    def apply[A](fa: InteractionDsl[A]) =
      fa match {
        case Say(text, voice) =>
          alert(text)
          ()
        case Receive =>
          prompt().asInstanceOf[A]
        case Ask(question) =>
          prompt(question).asInstanceOf[A]
      }

  }

}
