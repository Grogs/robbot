import cats.free.Free

import scala.util.Random

class Standup {

  val nextTicketResponses = Seq("what about this one?", "Okay lets talk about this now")
  val tooLongResponses = Seq("Maybe we should talk about this after the standup", "Lets take this offline", "Okay lets talk about this later")

  import language._

  val theStandup: Free[StandupDsl, Unit] = for {
    _ <- Say("Good morning everyone").lift
    jacekStatus <- Ask("How are you today yaseck?").lift
    jacekResponse = if (jacekStatus == "g") "Great. Glad to hear it!" else "Oh dear!"
    _ <- Say(jacekResponse).lift
    _ <- Ask("Where is Russel this morning?").lift
    _ <- Ask("Any updates to production?").lift
    _ <- Say("okay moving on").lift
    _ <- Say("Shall we talk about this ticket?").lift
    _ <- until("e") {
      for {
        in <- Receive.lift
        resp <- in match {
          case "n" => Say(Random.shuffle(nextTicketResponses).head).lift
          case "l" => Say(Random.shuffle(tooLongResponses).head).lift
          case "h" => Say("Well how do you feel about this?").lift
          case "e" =>
            Say("Okay great thanks everyone").lift
          case _ => Say("Sorry I didn't quite catch that?").lift
        }
      } yield in
    }
  } yield ()

}
