package abts

import prelude._

/** Our term language with binding removed */
sealed trait TermF[A]
object TermF {
  case class True[A]() extends TermF[A]
  case class False[A]() extends TermF[A]
  case class If[A](guard: A, thenCase: A, elseCase: A) extends TermF[A]
  case class App[A](left: A, right: A) extends TermF[A]

  // I heard some langauges write this for you.
  implicit object TermFFoldable extends Foldable[TermF] {
    override def map[A, B](context: TermF[A])(f: A => B) =
      context match {
        case True() => True()
        case False() => False()
        case If(guard, thenCase, elseCase) => If(f(guard), f(thenCase), f(elseCase))
        case App(left, right) => App(f(left), f(right))
      }

    override def fold[A](context: TermF[A])(implicit monoid: Monoid[A]): A =
      context match {
        case True() => monoid.zero
        case False() => monoid.zero
        case If(guard, thenCase, elseCase) => monoid.plus(monoid.plus(guard, thenCase), elseCase)
        case App(left, right) => monoid.plus(left, right)
      }
  }
}

/** Our types */
sealed trait Type
object Type {
  case object Bool extends Type
  case class Arrow(ty1: Type, ty2: Type) extends Type
}

/** Our term language */
sealed trait Term
object Term {
  case object True extends Term
  case object False extends Term
  case class If(guard: Term, thenCase: Term, elseCase: Term) extends Term
  case class Abs(name: String, body: Term) extends Term
  case class App(left: Term, right: Term) extends Term
  case class Var(name: String) extends Term

  def toABT(xterm: Term): ABT[TermF] =
    xterm match {
      case True =>
        ABT.Wrap(TermF.True())
      case False =>
        ABT.Wrap(TermF.False())
      case If(guard, thenCase, elseCase) =>
        ABT.Wrap(TermF.If(toABT(guard), toABT(thenCase), toABT(elseCase)))
      case Abs(name, body) =>
        ABT.Abs(name, toABT(body))
      case App(left, right) =>
        ABT.Wrap(TermF.App(toABT(left), toABT(right)))
      case Var(name) =>
        ABT.Var(name)
    }

  def fromABT(exp: ABT[TermF]): Term =
    exp match {
      case ABT.Var(name) => Term.Var(name)
      case ABT.Abs(name, body) => Term.Abs(name, fromABT(body))
      case ABT.Wrap(yterm) =>
        yterm match {
          case TermF.True() => Term.True
          case TermF.False() => Term.False
          case TermF.If(guard, thenCase, elseCase) =>
            Term.If(fromABT(guard), fromABT(thenCase), fromABT(elseCase))
          case TermF.App(left, right) => Term.App(fromABT(left), fromABT(right))
        }
    }

  def isValue(exp: ABT[TermF]): Boolean =
    exp match {
      case ABT.Var(_) | ABT.Abs(_, _) => true
      case ABT.Wrap(yterm) =>
        yterm match {
          case TermF.True() | TermF.False() => true
          case _ => false
        }
    }

  def smallStep(exp: ABT[TermF]): Option[ABT[TermF]] =
    exp match {
      case ABT.Wrap(yterm) =>
        yterm match {
          case TermF.If(guard, thenCase, elseCase) if !isValue(guard) =>
            smallStep(guard).map { g => ABT.Wrap(TermF.If(g, thenCase, elseCase)) }
          case TermF.If(ABT.Wrap(TermF.True()), thenCase, _) =>
            Some(thenCase)
          case TermF.If(ABT.Wrap(TermF.False()), _, elseCase) =>
            Some(elseCase)
          case TermF.App(left, right) if !isValue(right) =>
            smallStep(right).map { r => ABT.Wrap(TermF.App(left, r)) }
          case TermF.App(left, right) if !isValue(left) =>
            smallStep(left).map { l => ABT.Wrap(TermF.App(l, right)) }
          case TermF.App(ABT.Abs(name, body), value) =>
            Some(ABT.substitute(name, value, body))
          case _ => None
        }
      case _ => None
    }

  @scala.annotation.tailrec
  def bigStep(exp: ABT[TermF]): ABT[TermF] =
    smallStep(exp) match {
      case Some(next) => bigStep(next)
      case None => exp
    }
}