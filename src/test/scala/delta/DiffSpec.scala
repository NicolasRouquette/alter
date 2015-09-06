package delta

import alter.{Diff, Patcher}
import org.scalacheck._
import org.scalacheck.Prop.{forAll, BooleanOperators}

object DiffSpec extends Properties("DiffSpec") {

  property("int should converge") = forAll { (a: Int, b: Int) => converge(a, b) }
  property("string should converge") = forAll { (a: String, b: String) => converge(a, b) }
  property("list should converge") = forAll { (a: List[Int], b: List[Int]) => (a.size < 100 && b.size < 100) ==> converge(a,b) }
  property("map should converge") = forAll { (a: Map[String, String], b: Map[String, String]) => converge(a, b) }

  def converge[A](x: A, y: A)(implicit diff: Diff[A], patcher: Patcher[A]): Boolean = {
    patcher.patch(diff.diff(x, y), x) match {
      case Left(_) => false
      case Right(to) => to == y
    }
  }

}
