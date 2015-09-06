package alter

import shapeless.labelled._
import shapeless._

import scala.annotation.tailrec
import scala.collection.immutable.{:: => **}
import scala.reflect.ClassTag

trait Patcher[A] {
  def patch(script: EditScript, src: A): Either[String, A]
}

object Patcher {

  implicit val int: Patcher[Int] = new Patcher[Int] {
    override def patch(script: EditScript, src: Int): Either[String, Int] = script match {
      case Update(to: Int) => Right(to)
      case Copy(to: Int) => Right(to)
      case _ => Left("Unable to converge")
    }
  }

  implicit val string: Patcher[String] = new Patcher[String] {
    override def patch(script: EditScript, src: String): Either[String, String] = script match {
      case Update(to: String) => Right(to)
      case Copy(to: String) => Right(to)
      case _ => Left("Unable to converge")
    }
  }

  implicit def list[A : ClassTag]: Patcher[List[A]] = new Patcher[List[A]] {
    override def patch(script: EditScript, src: List[A]): Either[String, List[A]] = {
      @tailrec
      def iterate(s: List[EditScript], acc: List[A]): Option[List[A]] = s match {
        case x ** xs => step(x, acc) match {
          case None => None
          case Some(r) => iterate(xs, r)
        }
        case Nil => Some(acc)
      }

      def step(s: EditScript, acc: List[A]): Option[List[A]] = s match {
        case Changes(scripts) => iterate(scripts, acc)
        case Insert(elem: A) => Some(elem +: acc)
        case Delete(elem: A) => Some(acc)
        case Copy(elem: A) => Some(elem +: acc)
        case Nothing => Some(acc)
        case _ => None
      }

      step(script, List.empty) match {
        case None => Left("Unable to fold")
        case Some(result) => Right(result)
      }
    }
  }

  implicit def mapPatcher[K : ClassTag, V : ClassTag]: Patcher[Map[K, V]] = new Patcher[Map[K, V]] {
    override def patch(script: EditScript, src: Map[K, V]): Either[String, Map[K, V]] = {
      @tailrec
      def iterate(s: List[EditScript], acc: Map[K, V]): Option[Map[K, V]] = s match {
        case x ** xs => step(x, acc) match {
          case None => None
          case Some(r) => iterate(xs, r)
        }
        case Nil => Some(acc)
      }

      def step(s: EditScript, acc: Map[K, V]): Option[Map[K, V]] = s match {
        case Changes(scripts) => iterate(scripts, acc)
        case Named(key: K, Insert(elem: V)) => Some(acc + (key -> elem))
        case Named(key: K, Delete(elem: V)) => Some(acc - key)
        case Named(key: K, Copy(elem: V)) => Some(acc)
        case Named(key: K, Update(elem: V)) => Some(acc.updated(key, elem))
        case Nothing => Some(acc)
        case _ => None
      }

      step(script, src) match {
        case None => Left("Unable to fold")
        case Some(result) => Right(result)
      }
    }
  }

  implicit val patcherHNil: Patcher[HNil] = new Patcher[HNil] {
    override def patch(script: EditScript, src: HNil): Either[String, HNil] = Right(HNil)
  }

  implicit def patcherHCon[K <: Symbol, V, T <: HList](implicit witness: Witness.Aux[K],
    patcherV: Lazy[Patcher[V]],
    patcherT: Lazy[Patcher[T]]): Patcher[FieldType[K, V] :: T] = {
    new Patcher[FieldType[K, V] :: T] {
      override def patch(script: EditScript, src: ::[FieldType[K, V], T]): Either[String, ::[FieldType[K, V], T]] = script match {
        case Changes(changes) =>
          def getV = {
            changes find {
              case Named(key: String, _) if key == witness.value.name => true
              case _ => false
            } collect {
              case Named(_, change) => patcherV.value.patch(change, src.head)
            } getOrElse Left("error")
          }

          for {
            v <- getV.right
            t <- patcherT.value.patch(Changes(changes), src.tail).right
          } yield field[K](v) :: t
        case _ => Left("Error")
      }
    }
  }

  implicit def patcherGen[T, R](implicit
    gen: LabelledGeneric.Aux[T, R],
    patcherRepr: Lazy[Patcher[R]]): Patcher[T] = new Patcher[T] {
    override def patch(script: EditScript, src: T): Either[String, T] = patcherRepr.value.patch(script, gen.to(src)).right.map(gen.from)
  }

  def apply[T](implicit a: Patcher[T]): Patcher[T] = a
}