package alter

sealed trait Tree[A]
case class Node[A](a: Tree[A], b: Tree[A]) extends Tree[A]
case class Leaf[A](value: A) extends Tree[A]

case class Person(id: Int, name: String)

object TestApp  extends App {

  val karel = Person(1, "Karel")
  val dirk = Person(2, "Dirk")
  val mark = Person(3, "Mark")
  val frits = Person(4, "Frits")
  val klaas = Person(5, "Klaas")

  val xs = List(karel, dirk, frits, mark)
  val ys = List(klaas, mark, dirk, frits)


  println(Diff[List[Person]].diff(xs, ys))
//  println(Patcher[Tree[Int]].patch(script, x))
}
