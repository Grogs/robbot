import scala.sys.process._
import cats.free.Free.liftF
import cats.free.Free

import cats.{Id, ~>}

object Rob extends App {

    val nextTicketResponses = Seq("Next Ticket", "what about this one?", "Okay lets talk about this now")
    val tooLongResponses = Seq("Maybe we should talk about this after the standup", "Lets take this offline", "Okay lets talk about this later")


    sealed trait Interaction[Output]
    case class Say(text: String) extends Interaction[Unit]
    case class Receive() extends Interaction[String]
    case class Ask(question: String) extends Interaction[String]

    implicit class Lift[F[_],A](fa: F[A]) {
        def lift: Free[F,A] = Free.liftF(fa)
    }

//    def untilM[F,R](term: R)(m: Free[F,R]) =
//        m.flatMap(res =>
//            if (res == term)
//                Free.pure(res)
//            else
//                m
//        )


    val program: Free[Interaction, Unit] = for {
        _ <- Say("-v Cellos Good morning everyone").lift
        jacekStatus <- Ask("How are you today yaseck?").lift
        jacekResponse =
            if (jacekStatus == "g")
                "Great glad to hear it"
            else
                "Oh dear"
        _ <- Ask("Where is Russel this morning?").lift
        _ <- Ask("Any updates to production?").lift
        _ <- Say("okay moving on").lift
        _ <- Say("Lets talk about this ticket").lift
//        _ <- untilM("e") {
//            for {
//                in <- Receive().lift
//                resp <- in match {
//                    case "n" => Say(nextTicketResponses(util.Random.nextInt(2)))
//                    case "l" => Say(tooLongResponses(util.Random.nextInt(2)))
//                    case "h" => Say("Well how do you feel about this?")
//                    case "r" => Say("Yacek could you put a red dot on that")
//                    case "e" =>
//                        Say("Okay great thanks everyone")
//                        run = false
//                    case _   => Say("Sorry I didn't quite catch that?")
//                }
//            } yield in
//        }
    } yield ()


    def impureCompiler: Interaction ~> Id  =
        new (Interaction ~> Id) {

            def say(thing: String) = {
                s"say ${thing}".!!
                println(thing)
            }

            def apply[A](fa: Interaction[A]) =
                fa match {
                    case Say(text) =>
                        say(text)
                        ()
                    case Receive() =>
                        readLine().asInstanceOf[A]
                    case Ask(question) =>
                        say(question)
                        readLine().asInstanceOf[A]
                }
        }


    program.foldMap(impureCompiler)

//    var run = true
//
//    while (run) {
//
//        val lResponse = readLine()
//
//        lResponse match {
//            case "n" => say(nextTicketResponses(util.Random.nextInt(2)))
//            case "l" => say(tooLongResponses(util.Random.nextInt(2)))
//            case "h" => say("Well how do you feel about this?")
//            case "r" => say("Yacek could you put a red dot on that")
//            case "e" =>
//                say("Okay great thanks everyone")
//                run = false
//            case _   => say("Sorry I didn't quite catch that?")
//        }
//
//    }
}