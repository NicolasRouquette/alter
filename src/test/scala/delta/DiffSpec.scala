package delta

import alter.{Diff, Patcher}
import org.scalacheck.Prop.forAll
import org.scalacheck.Shapeless._
import org.scalacheck._

sealed trait Gender
case object Male extends Gender
case object Female extends Gender

case class Address(street: String, houseNumber: String, city: String, country: String)
case class Person(id: Int, name: String, address: Option[Address], weight: Double, isAlive: Boolean)

sealed trait Tree[A]
case class Node[A](left: Tree[A], right: Tree[A]) extends Tree[A]
case class Leaf[A](value: A) extends Tree[A]

object DiffSpec extends Properties("DiffSpec") {

  implicit val arbAddress = implicitly[Arbitrary[Address]]
  implicit val arbPerson = implicitly[Arbitrary[Person]]
  implicit val arbTree = implicitly[Arbitrary[Tree[Int]]]

  property("bool should converge") = forAll { (a: Int, b: Int) => converge(a, b) }
  property("int should converge") = forAll { (a: Int, b: Int) => converge(a, b) }
  property("float should converge") = forAll { (a: Float, b: Float) => converge(a, b) }
  property("double should converge") = forAll { (a: Double, b: Double) => converge(a, b) }
  property("short should converge") = forAll { (a: Short, b: Short) => converge(a, b) }
  property("long should converge") = forAll { (a: Long, b: Long) => converge(a, b) }
  property("byte should converge") = forAll { (a: Byte, b: Byte) => converge(a, b) }
  property("string should converge") = forAll { (a: String, b: String) => converge(a, b) }
  property("option should converge") = forAll { (a: Option[Int], b: Option[Int]) => converge(a,b) }
  property("either should converge") = forAll { (a: Either[Int, Int], b: Either[Int, Int]) => converge(a,b) }
  property("list should converge") = forAll { (a: Seq[Int], b: Seq[Int]) => converge(a,b) }
  property("set should converge") = forAll { (a: Set[Int], b: Set[Int]) => converge(a,b) }
  property("map should converge") = forAll { (a: Map[String, String], b: Map[String, String]) => converge(a, b) }
  property("product type should converge") = forAll { (a: Person, b: Person) => converge(a, b) }
//  property("coproduct type should converge") = forAll { (a: Tree[Int], b: Tree[Int]) => converge(a, b) }

  def converge[A](x: A, y: A)(implicit diff: Diff[A], patcher: Patcher[A]): Boolean = {
    val script = diff.diff(x, y)
//    println(s"script: $script, target: $y")
    patcher.patch(script, x) match {
      case Left(_) => false
      case Right(to) =>
//        println(s"to: $to, equal: ${to == y}")
        to == y
    }
  }

}
