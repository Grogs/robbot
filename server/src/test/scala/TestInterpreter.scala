import cats.{Id, ~>}
import language.{Ask, Receive, Say, StandupDsl}

import scala.collection.mutable

class TestInterpreter(input: List[String]) extends (StandupDsl ~> Id) {

  val result = mutable.ListBuffer[String]()
  val receiveQueue = mutable.Queue(input.toArray: _*)

  def apply[A](cmd: StandupDsl[A]) = cmd match {

    case Say(text) =>
      result += text
      ()

    case Receive =>
      receiveQueue.dequeue().asInstanceOf[A]

    case Ask(question) =>
      result += question
      receiveQueue.dequeue().asInstanceOf[A]
  }

}

