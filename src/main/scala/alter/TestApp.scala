package alter

sealed trait Tree[A]
case class Node[A](a: Tree[A], b: Tree[A]) extends Tree[A]
case class Leaf[A](value: A) extends Tree[A]

//Primitives - Update | Nothing

//Product - [Named(k, v)]
//Coproduct - Update | Nothing

//Index based sets - Insert(at: Int, value: A), Remove(at: Int), Move(from: Int, to: Int)
//Maps - Insert(k: K, v: V), Remove(k: K)
//Option - Update | Nothing
//Either/Xor/Conjuction - Update | Nothing
//Set - Insert(v: V), Remove(v: V)


case class Person(id: Int, name: String, roleIds: Seq[Int])

object TestApp extends App {

  val karel = Person(1, "Karel", List(1,2,3))
  val dirk = Person(2, "Dirk", List(2,3,4,5))

  println(Diff[Person].diff(karel, dirk))
}
