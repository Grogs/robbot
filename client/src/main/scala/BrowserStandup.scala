import cats.{Id, ~>}
import language.{Ask, Receive, Say, StandupDsl}

import scala.scalajs.js.Dynamic.global
import scala.scalajs.js.JSApp
import scala.scalajs.js.annotation.JSExport

object BrowserStandup extends JSApp {

  @JSExport
  override def main(): Unit = {
    val standup = new Standup().theStandup
    standup.foldMap(BrowserInteractionInterpreter)
  }

  object BrowserInteractionInterpreter extends (StandupDsl ~> Id) {

    def apply[A](cmd: StandupDsl[A]) = cmd match {

      case Say(text, voice) =>
        alert(text)
        ()

      case Receive =>
        prompt().asInstanceOf[A]

      case Ask(question) =>
        prompt(question).asInstanceOf[A]
    }

    def prompt(text: String = "") = {
      println(text)
      global.prompt(text).asInstanceOf[String]
    }

    def alert(text: String) = {
      global.alert(text).asInstanceOf[Unit]
    }


  }

}
