import cats.free.Free
import cats.{Id, ~>}
import language.{Ask, Closed, GetTickets, InteractionDsl, Receive, Say, SetStatus, StandupDsl, TicketDsl}

import scala.sys.process._


object OsxStandup extends App {
  val interpreter = new StandupInterpreter(OsxInteractionInterpreter, JiraInterpreter)

  def run[Res](standup: Free[StandupDsl, Res], interpreter: StandupInterpreter): Res =
    standup.foldMap(interpreter)

  run(new Standup().theStandup, interpreter)


  object OsxInteractionInterpreter extends (InteractionDsl ~> Id) {

    def say(text: String, voice: Option[String]) = {
      println(text)
      s"say -v ${voice.getOrElse("Alex")} $text".!!
    }

    def apply[A](fa: InteractionDsl[A]) =
      fa match {
        case Say(text, voice) =>
          say(text, voice)
          ()
        case Receive =>
          readLine().asInstanceOf[A]
        case Ask(question) =>
          say(question, None)
          readLine().asInstanceOf[A]
      }

  }

  object JiraInterpreter extends (TicketDsl ~> Id) {
    def apply[A](instr: TicketDsl[A]) = instr match {
      case GetTickets =>
        Seq("1. Implement standup with bash script" -> Closed).asInstanceOf[A]
      case SetStatus(ticket: String, newStatus) =>
        println(s"Transitioning $ticket to $newStatus").asInstanceOf[A]
    }
  }

}


