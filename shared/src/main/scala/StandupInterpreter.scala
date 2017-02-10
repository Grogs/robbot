import cats.{Id, ~>}
import language.{InteractionDsl, StandupDsl, TicketDsl}

/**
  * Created by Greg Dorrell on 08/02/2017.
  */
class StandupInterpreter(
                          interactionsHandler: InteractionDsl ~> Id,
                          ticketHandler: TicketDsl ~> Id
                        ) extends (StandupDsl ~> Id) {
  def apply[A](fa: StandupDsl[A]) = fa match {
    case instr: InteractionDsl[A] => interactionsHandler(instr)
    case instr: TicketDsl[A] => ticketHandler(instr)
  }
}
