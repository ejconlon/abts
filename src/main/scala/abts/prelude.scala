package abts

trait Functor[F[_]] {
  def map[A, B](context: F[A])(f: A => B): F[B]
}

trait Monoid[A] {
  def zero: A
  def plus(l: A, r: A): A
}

trait Foldable[F[_]] extends Functor[F] {
  def fold[A](context: F[A])(implicit monoid: Monoid[A]): A
  def foldMap[A, B](context: F[A])(f: A => B)(implicit monoid: Monoid[B]): B =
    fold(map(context)(f))
}

object Monoid {
  class SetMonoid[A] extends Monoid[Set[A]] {
    override val zero = Set.empty[A]
    override def plus(l: Set[A], r: Set[A]) = l ++ r
  }

  class VectorMonoid[A] extends Monoid[Vector[A]] {
    override val zero = Vector.empty[A]
    override def plus(l: Vector[A], r: Vector[A]) = l ++ r
  }

  implicit def setMonoid[A]: Monoid[Set[A]] = new SetMonoid[A]

  implicit def vectorMonoid[A]: Monoid[Vector[A]] = new VectorMonoid[A]
}