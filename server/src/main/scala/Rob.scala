import cats.free.Free
import cats.kernel.Monoid
import cats.{Applicative, Foldable, Id, Monad, MonadCombine, MonoidK, Semigroup, SemigroupK, ~>}
import language.{Ask, Closed, GetTickets, InteractionDsl, Receive, Say, SetStatus, StandupDsl, TicketDsl}

import scala.sys.process._
import scala.util.Random

class Standup {

    val nextTicketResponses = Seq("Next Ticket", "what about this one?", "Okay lets talk about this now", "Okay moving on, I haven't seen this many red dots since Nicola Sturgeon")
    val tooLongResponses = Seq("Maybe we should talk about this after the standup", "Lets take this offline", "Okay lets talk about this later")

    import language._

    val theStandup: Free[StandupDsl, Unit] = for {
        _ <- Say("Good morning everyone", Some("Cellos")).lift
//        jacekStatus <- Ask("How are you today yaseck?").lift
//        jacekResponse = if (jacekStatus == "g") "Great. Glad to hear it!" else "Oh dear!"
//        _ <- Say(jacekResponse).lift
//        _ <- Ask("Where is Russel this morning?").lift
        _ <- Ask("Any updates to production?").lift
        _ <- Say("okay moving on").lift
        _ <- Say("Shall we talk about this ticket?").lift
        _ <- untilF("e") {
            for {
                in <- Receive.lift
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


    def run(interpreter: StandupInterpreter) =
        theStandup.foldMap(interpreter)

}
object Standup extends App {
    val interpreter = new StandupInterpreter(OsxInteractionInterpreter, JiraInterpreter)

    new Standup().run(interpreter)
}



class StandupInterpreter(
                          interactionsHandler: InteractionDsl ~> Id,
                          ticketHandler: TicketDsl ~> Id
                        ) extends (StandupDsl ~> Id) {
    def apply[A](fa: StandupDsl[A]) = fa match {
        case instr: InteractionDsl[A] => interactionsHandler(instr)
        case instr: TicketDsl[A] => ticketHandler(instr)
    }
}

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

object language {

    sealed trait StandupDsl[Output]

    sealed trait InteractionDsl[Output] extends StandupDsl[Output]
    case class Say(text: String, voice: Option[String] = None) extends InteractionDsl[Unit]
    case object Receive extends InteractionDsl[String]
    case class Ask(question: String) extends InteractionDsl[String]


    sealed class TicketStatus(position: Int)
    case object Backlog extends TicketStatus(0)
    case object Analysis extends TicketStatus(1)
    case object Development extends TicketStatus(2)
    case object Review extends TicketStatus(3)
    case object Verifcation extends TicketStatus(4)
    case object Deployed extends TicketStatus(5)
    case object Closed extends TicketStatus(6)

    sealed trait TicketDsl[Output] extends StandupDsl[Output]
    case object GetTickets extends TicketDsl[Seq[(String, TicketStatus)]]
//    case class GetStatus(ticketId: String) extends TicketDsl[TicketStatus]
    case class SetStatus(ticketId: String, newStatus: TicketStatus) extends TicketDsl[Unit]

    class Lift[F[_], A](fa: F[A]) {
        def lift: Free[F, A] = Free.liftF(fa)
    }

    implicit class ForComprehensionSugar[Instruction <: StandupDsl[Output], Output](instruction: StandupDsl[Output]) extends Lift[StandupDsl, Output](instruction)

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

//    def untilM2[R, M[_] : Monad, G[R]: MonadCombine: Semigroup](term: R)(m: => M[R]): M[G[R]] = {
//        import cats.syntax.all._
//        val M = implicitly[Monad[M]]
//        val MC = implicitly[MonadCombine[G]]
//        M.flatMap(m.map(MC.pure))(res =>
//            if (res == term)
//                M.pure(res)
//            else
//                untilM2(term)(m).map(_.combine(res))
//        )
//    }

//    def untilFC[T[_], Res, Mon[_]:Applicative:SemigroupK](term: Res)(m: => Free[T, Res]): Free[T, Mon[Res]] = {
//        val mon = implicitly[Applicative[Mon]]
//        val sg = implicitly[SemigroupK[Mon]]
//        m.flatMap(res =>
//            if (res == term)
//                Free.pure(mon.pure(res))
//            else
//                untilFC(term)(m).map( t=> sg.combineK(t, res))
//        )
//    }

}
