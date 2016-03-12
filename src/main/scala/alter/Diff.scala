package alter

import cats.Monoid
import cats.data.Xor
import cats.functor.Contravariant
import cats.syntax.all._
import shapeless._
import shapeless.labelled._

import scala.annotation.tailrec
import scala.collection.immutable.{:: => **}


sealed trait EditScript

object EditScript {

  final case class SetInsert[A](elem: A) extends EditScript
  final case class SetRemove[A](elem: A) extends EditScript
  final case class MapInsert[K, V](key: K, value: V) extends EditScript
  final case class MapRemove[K](key: K) extends EditScript
  final case class ListInsert[A](at: Int, value: A) extends EditScript
  final case class ListRemove(at: Int) extends EditScript
  final case class ListMove(from: Int, to: Int) extends EditScript
  final case class Changes(script: List[EditScript]) extends EditScript
  final case class Update[A](to: A) extends EditScript
  final case class Named[A](name: A, script: EditScript) extends EditScript
  case object Nothing extends EditScript


  implicit val monoid = new Monoid[EditScript] {
    def empty = Nothing
    def combine(left: EditScript, right: EditScript) = (left, right) match {
      case (Changes(x), Changes(y)) => Changes(x ++ y)
      case (Changes(x), o) => Changes(x :+ o)
      case (o, Changes(x)) => Changes(o +: x)
      case (Nothing, Nothing) => Nothing
      case (Nothing, y) => y
      case (x, Nothing) => x
      case (x, y) => Changes(List(x, y))
    }
  }
}

trait Diff[A] {
  def diff(source: A, target: A): EditScript
}


object Diff {

  def apply[A](f: (A, A) => EditScript): Diff[A] = new Diff[A] {
    override def diff(source: A, target: A): EditScript = f(source, target)
  }

  private def primitiveDiff[A]: Diff[A] = Diff((src, tar) => if (src == tar) EditScript.Nothing else EditScript.Update(tar))

  implicit val bool = primitiveDiff[Boolean]
  implicit val int = primitiveDiff[Int]
  implicit val double = primitiveDiff[Double]
  implicit val float = primitiveDiff[Float]
  implicit val long = primitiveDiff[Long]
  implicit val short = primitiveDiff[Short]
  implicit val byte = primitiveDiff[Byte]
  implicit val string = primitiveDiff[String]


  implicit def eitherDiff[L, R](implicit diffL: Diff[L], diffR: Diff[R]) = new Diff[Either[L, R]] {
    override def diff(source: Either[L, R], target: Either[L, R]): EditScript = (source, target) match {
      case (Left(l1), Left(l2)) => diffL.diff(l1, l2)
      case (Right(r1), Right(r2)) => diffR.diff(r1, r2)
      case (Left(l1), Right(r1)) => EditScript.Update(Right(r1))
      case (Right(r1), Left(l1)) => EditScript.Update(Left(l1))
    }
  }

  implicit def optionDiff[A](implicit deltaA: Diff[A]): Diff[Option[A]] = new Diff[Option[A]] {
    override def diff(source: Option[A], target: Option[A]): EditScript = (source, target) match {
      case (Some(x), Some(y)) =>
        def allCopies(delta: List[EditScript]): Boolean = {
          delta.forall {
            case EditScript.Changes(xs) => allCopies(xs)
            case EditScript.Named(_, more) => allCopies(List(more))
            case EditScript.Nothing => true
            case _ => false
          }
        }
        if (allCopies(List(deltaA.diff(x, y)))) EditScript.Nothing
        else EditScript.Update(Some(y))

      case (None, None) => EditScript.Nothing
      case (Some(x), None) => EditScript.Update(None)
      case (None, Some(y)) => EditScript.Update(Some(y))
    }
  }

  implicit def mapDiff[K, V](implicit diffV: Diff[V]): Diff[Map[K, V]] = new Diff[Map[K, V]] {
    def diff(source: Map[K, V], target: Map[K, V]): EditScript = {
      val removedKeys = source.keySet -- target.keySet
      val addedKeys = target.keySet -- source.keySet

      val removes: List[EditScript] = removedKeys.map(k => EditScript.MapRemove(k)).toList
      val adds: List[EditScript] = addedKeys.flatMap(k => target.get(k).map(v => EditScript.MapInsert(k, v))).toList

      EditScript.Changes(removes ++ adds)
    }
  }

  implicit def listDiff[A]: Diff[List[A]] = new Diff[List[A]] {
    override def diff(source: List[A], target: List[A]): EditScript = {
      val sourceIndices = source.zipWithIndex.toMap
      val targetIndices = source.zipWithIndex.toMap

      val deletes: List[EditScript] = source.filterNot(targetIndices.contains).flatMap(sourceIndices.get).map(EditScript.ListRemove)
      val inserts: List[EditScript] = target.filterNot(sourceIndices.contains).flatMap(x => targetIndices.get(x).map(_ -> x)).map { case (idx, v) => EditScript.ListInsert(idx, v) }
      val moves: List[EditScript] = source
        .zipWithIndex
        .filterNot { case (el, _) => deletes.contains(el) }
        .flatMap { case (el, prevIndex) => targetIndices.get(el).map(targetIndex => prevIndex -> targetIndex) }
        .filter { case (previousIndex, currentIdx) => currentIdx != previousIndex }
        .map { case (previousIndex, currentIdx) => EditScript.ListMove(currentIdx, previousIndex) }

      EditScript.Changes(deletes ++ inserts ++ moves)
    }
  }

  implicit def setDiff[A]: Diff[Set[A]] = new Diff[Set[A]] {
    override def diff(source: Set[A], target: Set[A]): EditScript = {
      val removedValues = source -- target
      val addedValues = target -- source
      val copiedValues = source.intersect(target)

      val removes: List[EditScript] = removedValues.map(EditScript.SetRemove(_)).toList
      val adds: List[EditScript] = addedValues.map(EditScript.SetInsert(_)).toList

      EditScript.Changes(removes ++ adds)
    }
  }

  implicit val hnilDiff: Diff[HNil] = new Diff[HNil] {
    def diff(source: HNil, target: HNil) = EditScript.Nothing
  }

  implicit def hconDiff[K <: Symbol, V, T <: HList](implicit witness: Witness.Aux[K],
                                                    diffV: Lazy[Diff[V]],
                                                    diffT: Lazy[Diff[T]]): Diff[FieldType[K, V] :: T] =
    Diff((src, tar) => EditScript.monoid.combine(EditScript.Named(witness.value.name, diffV.value.diff(src.head, tar.head)), diffT.value.diff(src.tail, tar.tail)))


  implicit val cnilDiff: Diff[CNil] = new Diff[CNil] {
    def diff(source: CNil, target: CNil) = EditScript.Nothing
  }

  implicit def cconDiff[K <: Symbol, V, T <: Coproduct](implicit witness: Witness.Aux[K],
    deltaV: Lazy[Diff[V]],
    deltaT: Lazy[Diff[T]]): Diff[FieldType[K, V] :+: T] = {
    new Diff[FieldType[K, V] :+: T] {
      def diff(source: FieldType[K, V] :+: T, target: FieldType[K, V] :+: T) = (source, target) match {
        case (Inl(x), Inl(y)) => deltaV.value.diff(x, y)
        case (Inr(x), Inr(y)) => deltaT.value.diff(x, y)
        case (_, Inr(y)) => EditScript.Update(y)
        case (_, Inl(y)) => EditScript.Update(y)
      }
    }
  }

  implicit def genDiff[T, R](implicit
    gen: LabelledGeneric.Aux[T, R],
    deltaRepr: Lazy[Diff[R]]): Diff[T] = new Diff[T] {
    override def diff(source: T, target: T) = deltaRepr.value.diff(gen.to(source), gen.to(target))
  }

  implicit def contravariant = new Contravariant[Diff] {
    def contramap[A, B](fa: Diff[A])(f: (B) => A) = Diff((src, tar) => fa.diff(f(src), f(tar)))
  }

  def apply[T](implicit a: Diff[T]): Diff[T] = a
}
