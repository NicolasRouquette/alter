package alter

import shapeless.labelled._
import shapeless._

import scala.annotation.tailrec
import scala.collection.immutable.{:: => **}
import scala.util.control.TailCalls._


sealed trait EditScript { self =>

  def +(s: EditScript): EditScript = (self, s) match {
    case (Changes(x), Changes(y)) => Changes(x ++ y)
    case (Changes(x), o) => Changes(x :+ o)
    case (o, Changes(x)) => Changes(o +: x)
    case (Nothing, Nothing) => Nothing
    case (Nothing, y) => y
    case (x, Nothing) => x
    case (x, y) => Changes(List(x,y))
  }

  def cost: Int = {
    def costOperation(s: EditScript): Int = {
      @tailrec
      def loop(changes: List[EditScript], cost: Int): Int = changes match {
        case x ** xs => loop(xs, cost + costOperation(x))
        case Nil => 0
      }

      s match {
        case Changes(changes) => 0 //loop(changes, 0) * -1
        case Named(_, x) => costOperation(x)
        case Nothing => 0
        case _ => 1
      }
    }

    costOperation(self)
  }

  def longest(other: EditScript) = if(self.cost <= other.cost) self else other

  def isSame = self match {
    case Copy(_) => true
    case _ => false
  }
}
case class Insert[A](elem: A) extends EditScript
case class Delete[A](elem: A) extends EditScript
case class Changes(script: List[EditScript]) extends EditScript
case class Update[A](to: A) extends EditScript
case class Named[A](name: A, script: EditScript) extends EditScript
case class Copy[A](value: A) extends EditScript
case object Nothing extends EditScript

trait Diff[A] {
  def diff(source: A, target: A): EditScript
}


object Diff {
  implicit def opt[A](implicit deltaA: Diff[A]): Diff[Option[A]] = new Diff[Option[A]] {
    override def diff(source: Option[A], target: Option[A]): EditScript = (source, target) match {
      case (Some(x), Some(y)) =>
        def allCopies(delta: List[EditScript]): Boolean = {
          delta.forall {
            case Changes(xs) => allCopies(xs)
            case Named(_, more) => allCopies(List(more))
            case Copy(_) => true
            case _ => false
          }
        }
        if(allCopies(List(deltaA.diff(x, y)))) Copy(Some(x))
        else Update(Some(y))

      case (None, None) => Copy(None)
      case (Some(x), None) => Update(None)
      case (None, Some(y)) => Update(Some(y))
    }
  }

  implicit val int: Diff[Int] = new Diff[Int] {
    def diff(source: Int, target: Int) = if(source == target) Copy(source) else Update(target)
  }
  implicit val string: Diff[String] = new Diff[String] {
    def diff(source: String, target: String) = if(source == target) Copy(source) else Update(target)
  }

  implicit def deltaMap[K, V](implicit deltaV: Diff[V]): Diff[Map[K, V]] = new Diff[Map[K, V]] {
    def diff(source: Map[K, V], target: Map[K, V]): EditScript = {
      val removedKeys = source.keySet -- target.keySet
      val addedKeys = target.keySet -- source.keySet
      val sameKeys = source.keySet.intersect(target.keySet)

      val removes = removedKeys.foldLeft(Nothing : EditScript) { case (acc, v) => acc + Named(v, Delete(source(v))) }
      val adds = addedKeys.foldLeft(Nothing : EditScript) { case (acc, v) => acc + Named(v, Insert(target(v))) }
      val updates = sameKeys.foldLeft(Nothing : EditScript) { case (acc, v) => acc + Named(v, deltaV.diff(source(v), target(v))) }

      removes + updates + adds
    }
  }

  private case class Memo[A, B](cache: scala.collection.mutable.Map[A,B])(f: A => TailRec[B]) extends (A => TailRec[B]) {
    def apply(x: A) =
      cache.lift(x) match {
        case Some(res) => done(res)
        case None => f(x).flatMap {
          case res =>
            cache.update(x, res)
            done(res)
        }
      }
  }

  implicit def deltaList[A](implicit deltaA: Diff[A]): Diff[List[A]] = new Diff[List[A]] {
    override def diff(source: List[A], target: List[A]) = {

      type I = (List[A], List[A])
      type Dict = scala.collection.mutable.Map[I, EditScript]

      def loop(cache: Dict, acc: EditScript): Memo[I, EditScript] = Memo(cache) {
        case (Nil, Nil) => done(acc)
        case (Nil, y ** ys) => tailcall(loop(cache, Insert(y) + acc)(List.empty[A], ys))
        case (x ** xs, Nil) => tailcall(loop(cache, Delete(x) + acc)(xs, List.empty[A]))
        case (x ** xs, y ** ys) =>
          val delta = deltaA.diff(x, y)
          def best2 = for {
            a <- tailcall(loop(cache, Delete(x) + acc)(xs, y +: ys))
            b <- tailcall(loop(cache, Insert(y) + acc)(x +: xs, ys))
          } yield a longest b

          def best3 = for {
            a <- tailcall(loop(cache, Copy(x) + acc)(xs, ys))
            b <- best2
          } yield a longest b

          if(delta.cost < 0) tailcall(loop(cache, Changes(List(delta)))(xs,ys))
          if(x == y) best3
          else best2

      }
      loop(scala.collection.mutable.Map.empty, Nothing)(source, target).result
    }
  }

  implicit val deltaHNil: Diff[HNil] = new Diff[HNil] {
    def diff(source: HNil, target: HNil) = Nothing
  }

  implicit def deltaHCon[K <: Symbol, V, T <: HList](implicit witness: Witness.Aux[K],
    deltaV: Lazy[Diff[V]],
    deltaT: Lazy[Diff[T]]): Diff[FieldType[K, V] :: T] = {
    new Diff[FieldType[K, V] :: T] {
      def diff(source: FieldType[K, V] :: T, target: FieldType[K, V] :: T) = {
        Named(witness.value.name, deltaV.value.diff(source.head, target.head)) + deltaT.value.diff(source.tail, target.tail)
      }
    }
  }

  implicit val deltaCNil: Diff[CNil] = new Diff[CNil] {
    def diff(source: CNil, target: CNil) = Nothing
  }

  implicit def deltaCCon[K <: Symbol, V, T <: Coproduct](implicit witness: Witness.Aux[K],
    deltaV: Lazy[Diff[V]],
    deltaT: Lazy[Diff[T]]): Diff[FieldType[K, V] :+: T] = {
    new Diff[FieldType[K, V] :+: T] {
      def diff(source: FieldType[K, V] :+: T, target: FieldType[K, V] :+: T) = (source, target) match {
        case (Inl(x), Inl(y)) => deltaV.value.diff(x, y)
        case (Inr(x), Inr(y)) => deltaT.value.diff(x, y)
        case (_, Inr(y)) => Update(y)
        case (_, Inl(y)) => Update(y)
      }
    }
  }

  implicit def deltaGen[T, R](implicit
    gen: LabelledGeneric.Aux[T, R],
    deltaRepr: Lazy[Diff[R]]): Diff[T] = new Diff[T] {
    override def diff(source: T, target: T) = deltaRepr.value.diff(gen.to(source), gen.to(target))
  }

  def apply[T](implicit a: Diff[T]): Diff[T] = a
}
