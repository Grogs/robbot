import cats.free.Free

import scala.util.Random

class Standup {

    val nextTicketResponses = Seq("Next Ticket", "what about this one?", "Okay lets talk about this now", "Okay moving on, I haven't seen this many red dots since Nicola Sturgeon")
    val tooLongResponses = Seq("Maybe we should talk about this after the standup", "Lets take this offline", "Okay lets talk about this later")

    import language._

    val theStandup: Free[StandupDsl, Unit] = for {
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

}
