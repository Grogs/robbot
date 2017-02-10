import cats._
import cats.free.Free


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
