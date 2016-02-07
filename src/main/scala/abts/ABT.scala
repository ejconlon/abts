package abts

import prelude._

// I don't know of a better way to do this.  The compiler doesn't like this one:
// type Fix[F] = G[F, Fix[F]]
case class Fix[G[_[_], _], F[_]](unfix: G[F, Fix[G, F]])

sealed trait Bind[F[_], A]
object Bind {
  case class Var[F[_], A](name: String) extends Bind[F, A]
  case class Abs[F[_], A](name: String, body: Bind[F, A]) extends Bind[F, A]
  case class Wrap[F[_], A](unwrap: F[A]) extends Bind[F, A]
}

case class Node[F[_], A](freeVars: Set[String], bind: Bind[F, A])

object ABT {
  private[this] def fix[F[_]](bind: Bind[F, ABT[F]])(implicit foldable: Foldable[F]): ABT[F] =
    bind match {
      case Bind.Var(name) => Var(name)
      case Bind.Abs(name, body) => Abs(name, fix(body))
      case Bind.Wrap(unwrap) => Wrap(unwrap)
    }

  object Var {
    def apply[F[_]](name: String): ABT[F] =
      Fix(Node(Set(name), Bind.Var(name)))
    def unapply[F[_]](abt: ABT[F]): Option[String] =
      abt.unfix.bind match {
        case Bind.Var(name) => Some(name)
        case _ => None
      }
  }

  object Abs {
    def apply[F[_]](name: String, body: ABT[F]): ABT[F] =
      Fix(Node(body.unfix.freeVars - name, Bind.Abs(name, body.unfix.bind)))
    def unapply[F[_]](abt: ABT[F])(implicit foldable: Foldable[F]): Option[(String, ABT[F])] =
      abt.unfix.bind match {
        case Bind.Abs(name, body) => Some((name, fix(body)))
        case _ => None
      }
  }

  object Wrap {
    def apply[F[_]](unwrap: F[ABT[F]])(implicit foldable: Foldable[F]): ABT[F] =
      Fix(Node(foldable.foldMap(unwrap) { exp => freeVars(exp) }, Bind.Wrap(unwrap)))
    def unapply[F[_]](abt: ABT[F]): Option[F[ABT[F]]] =
      abt.unfix.bind match {
        case Bind.Wrap(unwrap) => Some(unwrap)
        case _ => None
      }
  }

  def freeVars[F[_]](exp: ABT[F])(implicit foldable: Foldable[F]): Set[String] =
    exp.unfix.freeVars

  private[this] def mapWrapped[F[_]](bind: Bind[F, ABT[F]])(f: ABT[F] => ABT[F])(implicit foldable: Foldable[F]): Bind[F, ABT[F]] =
    bind match {
      case Bind.Wrap(t) => Bind.Wrap(foldable.map(t)(f))
      case _ => bind
    }

  def substitute[F[_]](name: String, exp: ABT[F], body: ABT[F])(implicit foldable: Foldable[F]): ABT[F] =
    body.unfix.bind match {
      case Bind.Var(n) if (n == name) => exp
      case Bind.Abs(n, b) =>
        val freshName = fresh(name, exp.unfix.freeVars ++ body.unfix.freeVars)
        val freshBody: Bind[F, ABT[F]] =
          if (freshName != name) mapWrapped(b) { rename(name, freshName, _) }
          else b
        Abs(freshName, fix(mapWrapped(freshBody) { substitute(name, exp, _) }))
      case Bind.Wrap(t) => Wrap(foldable.map(t) { substitute(name, exp, _) })
      case _ => body
    }

  @scala.annotation.tailrec
  private[this] def fresh(name: String, freeVars: Set[String]): String =
    if (freeVars.contains(name)) fresh(name + "'", freeVars)
    else name

  private[this] def rename[F[_]](fromName: String, toName: String, body: ABT[F])(implicit foldable: Foldable[F]): ABT[F] =
    body.unfix.bind match {
      case Bind.Var(n) if (n == fromName) => Var(toName)
      case Bind.Abs(n, b) if (n != fromName) => Abs(n, rename(fromName, toName, fix(b)))
      case Bind.Wrap(t) => Wrap(foldable.map(t) { rename(fromName, toName, _) })
      case _ => body
    }
}

