import cats.free.Free


object language {

  sealed trait StandupDsl[Output]

  case class Say(text: String) extends StandupDsl[Unit]
  case object Receive extends StandupDsl[String]
  case class Ask(question: String) extends StandupDsl[String]


  implicit class LiftSugar[Output](instruction: StandupDsl[Output]) {
    def lift: Free[StandupDsl, Output] = Free.liftF(instruction)
  }

  def until[T](term: T)(m: => Free[StandupDsl, T]): Free[StandupDsl, T] = {
    m.flatMap(res =>
      if (res == term)
        Free.pure(res)
      else
        until(term)(m)
    )
  }

}
