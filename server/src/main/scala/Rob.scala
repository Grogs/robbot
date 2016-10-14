import scala.sys.process._
import cats.free.Free.liftF
import cats.free.Free

import cats.{Id, ~>}

object Rob extends App {

    sealed trait InteractionA[Output]
    case class Say(text: String) extends InteractionA[Unit]
    case class Receive() extends InteractionA[String]
    case class Ask(question: String) extends InteractionA[String]
    case class Respond(input: String, handler: String => String) extends InteractionA[Unit]


    type Interaction[A] = Free[InteractionA, A]


    def say(text: String): Interaction[Unit] =
        liftF[InteractionA, Unit](Say(text))

    def receive(): Interaction[String] =
        liftF[InteractionA, String](Receive())

    def respond(input: String)(handler: String => String): Interaction[Unit] =
        liftF[InteractionA, Unit](Respond(input, handler))

    def ask(question: String): Interaction[String] =
        liftF[InteractionA, String](Ask(question))

    val program = for {
        _ <- say("-v Cellos Good morning everyone")
        jacekStatus <- ask("How are you today yaseck?")
        _ <- respond(jacekStatus){
            case "g" => "Great glad to hear it"
            case _ => "Oh dear"
        }
        _ <- ask("Where is Russel this morning?")
        _ <- ask("Any updates to production?")
        _ <- say("okay moving on")
        _ <- say("Lets talk about this ticket")
        //need .whileM for loop over "next ticket" until standup is complete.
        //It's available in Scalaz: https://github.com/scalaz/scalaz/blob/series/7.3.x/core/src/main/scala/scalaz/Monad.scala#L21
        //But it's not yet in cats, as discussed here: https://github.com/typelevel/cats/pull/1216
    } yield ()


    def impureCompiler: InteractionA ~> Id  =
        new (InteractionA ~> Id) {

            def say(thing: String) = {
                s"say ${thing}".!!
                println(thing)
            }

            def apply[A](fa: InteractionA[A]) =
                fa match {
                    case Say(text) =>
                        say(text)
                        ()
                    case Receive() =>
                        readLine().asInstanceOf[A]
                    case Ask(question) =>
                        say(question)
                        readLine().asInstanceOf[A]
                    case Respond(input, handler) =>
                        say(handler(input))
                        ()
                }
        }


    program.foldMap(impureCompiler)

//    val nextTicketResponses = Seq("Next Ticket", "what about this one?", "Okay lets talk about this now")
//    val tooLongResponses = Seq("Maybe we should talk about this after the standup", "Lets take this offline", "Okay lets talk about this later")
//
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