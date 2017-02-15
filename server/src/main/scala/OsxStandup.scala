import cats.{Id, ~>}
import language.{Ask, Receive, Say, StandupDsl}

import scala.sys.process._


object OsxStandup extends App {

  val standup = new Standup().theStandup
  standup.foldMap(OsxInteractionInterpreter)


  object OsxInteractionInterpreter extends (StandupDsl ~> Id) {

    def apply[A](cmd: StandupDsl[A]) = cmd match {

        case Say(text) =>
          say(text)
          ()

        case Receive =>
          readLine().asInstanceOf[A]

        case Ask(question) =>
          say(question)
          readLine().asInstanceOf[A]
      }

    def say(text: String) = {
      println(text)
      s"say -v Alex $text".!!
    }

  }

}


