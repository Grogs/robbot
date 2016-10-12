object Rob extends App {
    import scala.sys.process._

    def say(thing: String) = {
        s"say ${thing}".!!
        println(thing)
    }

    say("-v Cellos Good morning everyone")

    say("How are you today yaseck?")
    readLine() match {
        case "g" => say("Great glad to hear it")
        case _ => say("Oh dear")
    }

    say("Where is Russel this morning?")
    readLine()

    say("Any updates to production?")
    readLine()

    say("okay moving on")

    say("Lets talk about this ticket")

    val nextTicketResponses = Seq("Next Ticket", "what about this one?", "Okay lets talk about this now")
    val tooLongResponses = Seq("Maybe we should talk about this after the standup", "Lets take this offline", "Okay lets talk about this later")

    var run = true

    while (run) {

        val lResponse = readLine()

        lResponse match {
            case "n" => say(nextTicketResponses(util.Random.nextInt(2)))
            case "l" => say(tooLongResponses(util.Random.nextInt(2)))
            case "h" => say("Well how do you feel about this?")
            case "r" => say("Yacek could you put a red dot on that")
            case "e" =>
                say("Okay great thanks everyone")
                run = false
            case _   => say("Sorry I didn't quite catch that?")
        }

    }
}