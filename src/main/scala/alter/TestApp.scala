package alter

object TestApp  extends App {

  case class Person(id: Int, name: String)

  val x = Person(1, "Mark")
  val y = Person(1, "Hans")
  val script = Diff[Person].diff(x,y)

  println(script)
  println(Patcher[Person].patch(script, x))
}
