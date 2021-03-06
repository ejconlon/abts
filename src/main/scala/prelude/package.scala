package object prelude {

  type Id[X] = X

  def id[X](x: X): X = x

  trait Functor[F[_]] {
    def map[A, B](context: F[A])(f: A => B): F[B]
  }

  trait Bifunctor[F[_, _]] {
    def dimap[A, B, Z, C](f: F[A, B])(l: Z => A)(r: B => C): F[Z, C]
    def lmap[A, B, Z](f: F[A, B])(l: Z => A): F[Z, B] = dimap[A, B, Z, B](f)(l)(id)
    def rmap[A, B, C](f: F[A, B])(r: B => C): F[A, C] = dimap[A, B, A, C](f)(id)(r)
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

    class SeqMonoid[A] extends Monoid[Seq[A]] {
      override val zero = Seq.empty[A]
      override def plus(l: Seq[A], r: Seq[A]) = l ++ r
    }

    implicit def setMonoid[A]: Monoid[Set[A]] = new SetMonoid[A]

    implicit def vectorMonoid[A]: Monoid[Vector[A]] = new VectorMonoid[A]

    implicit def seqMonoid[A]: Monoid[Seq[A]] = new SeqMonoid[A]
  }

}