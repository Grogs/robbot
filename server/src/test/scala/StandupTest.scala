object StandupTest extends App {

  val standup = new Standup().theStandup
  val interpreter = new TestInterpreter(List("g", "", "", "", "l", "n", "l", "e"))

  standup.foldMap(interpreter)

  assert(interpreter.receiveQueue.isEmpty)
  assert(interpreter.result.length == 12)

  interpreter.result.foreach(println)
}
