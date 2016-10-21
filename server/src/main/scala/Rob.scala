import cats.free.Free
import cats.{Id, Monad, ~>}
import language.{Ask, Interaction, Receive, Say}

import scala.sys.process._
import scala.util.Random

object Rob extends App {

    val nextTicketResponses = Seq("Next Ticket", "what about this one?", "Okay lets talk about this now", "Okay moving on, I haven't seen this many red dots since Nicola Sturgeon")
    val tooLongResponses = Seq("Maybe we should talk about this after the standup", "Lets take this offline", "Okay lets talk about this later")

    import language._

    val program: Free[Interaction, Unit] = for {
        _ <- Say("Good morning everyone", Some("Cellos")).lift
        jacekStatus <- Ask("How are you today yaseck?").lift
        jacekResponse = if (jacekStatus == "g") "Great. Glad to hear it!" else "Oh dear!"
        _ <- Say(jacekResponse).lift
        _ <- Ask("Where is Russel this morning?").lift
        _ <- Ask("Any updates to production?").lift
        _ <- Say("okay moving on").lift
        _ <- Say("Shall we talk about this ticket?").lift
        _ <- untilF("e") {
            for {
                in <- Receive().lift
                resp <- in match {
                    case "n" => Say(Random.shuffle(nextTicketResponses).head).lift
                    case "l" => Say(Random.shuffle(tooLongResponses).head).lift
                    case "h" => Say("Well how do you feel about this?").lift
                    case "r" => Say("Yacek could you put a red dot on that").lift
                    case "e" =>
                        Say("Okay great thanks everyone").lift
                    case _ => Say("Sorry I didn't quite catch that?").lift
                }
            } yield in
        }
    } yield ()



    program.foldMap(SpokenInterpreter)
}


object SpokenInterpreter extends (Interaction ~> Id) {

    def say(text: String, voice: Option[String]) = {
        println(text)
        s"say -v ${voice.getOrElse("Alex")} $text".!!
    }

    def apply[A](fa: Interaction[A]) =
        fa match {
            case Say(text, voice) =>
                say(text, voice)
                ()
            case Receive() =>
                readLine().asInstanceOf[A]
            case Ask(question) =>
                say(question, None)
                readLine().asInstanceOf[A]
        }

}

object language {

    sealed trait Interaction[Output]
    case class Say(text: String, voice: Option[String] = None) extends Interaction[Unit]
    case class Receive() extends Interaction[String]
    case class Ask(question: String) extends Interaction[String]

    implicit class Lift[F[_], A](fa: F[A]) {
        def lift: Free[F, A] = Free.liftF(fa)
    }

    def untilF[T[_], Res](term: Res)(m: => Free[T, Res]): Free[T, Res] = {
        type F[r] = Free[T, r]
        untilM[F, Res](term)(m)
    }

    def untilM[M[_] : Monad, R](term: R)(m: => M[R]): M[R] = {
        val M = implicitly[Monad[M]]
        M.flatMap(m)(res =>
            if (res == term)
                M.pure(res)
            else
                untilM(term)(m)
        )
    }

}
