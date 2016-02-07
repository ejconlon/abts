package abts2

import prelude._

/** Our term language with binding removed */
sealed trait TermF[A]

object TermF {
  case class True[A]() extends TermF[A]
  case class False[A]() extends TermF[A]
  case class If[A](guard: A, thenCase: A, elseCase: A) extends TermF[A]
  case class Abs[A](body: A) extends TermF[A]
  case class App[A](left: A, right: A) extends TermF[A]

  // I heard some langauges write this for you.
  implicit object TermFFoldable extends Foldable[TermF] {
    override def map[A, B](context: TermF[A])(f: A => B) =
      context match {
        case True() => True()
        case False() => False()
        case If(guard, thenCase, elseCase) => If(f(guard), f(thenCase), f(elseCase))
        case Abs(body) => Abs(f(body))
        case App(left, right) => App(f(left), f(right))
      }

    override def fold[A](context: TermF[A])(implicit monoid: Monoid[A]): A =
      context match {
        case True() => monoid.zero
        case False() => monoid.zero
        case If(guard, thenCase, elseCase) => monoid.plus(monoid.plus(guard, thenCase), elseCase)
        case Abs(body) => body
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

object Manual {
  import Term.{True, False, If, Abs, App, Var}

  def freeVars(term: Term): Seq[String] =
    term match {
      case True => Seq.empty
      case False => Seq.empty
      case If(guard, thenCase, elseCase) => Seq.empty
      case Abs(name, body) => freeVars(body).filterNot { _ == name }
      case App(left, right) => (freeVars(left) ++ freeVars(right)).distinct
      case Var(name) => Seq(name)
    }
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

  def toScope(xterm: Term): Scope[TermF] =
    xterm match {
      case True =>
        ABT.In[TermF](TermF.True()).scope(Seq.empty)
      case False =>
        ABT.In[TermF](TermF.False()).scope(Seq.empty)
      case If(guard, thenCase, elseCase) =>
        ABT.In[TermF](TermF.If(toScope(guard), toScope(thenCase), toScope(elseCase))).scope(Seq.empty)
      case Abs(name, body) =>
        ABT.In[TermF](TermF.Abs(toScope(body))).scope(Seq(name))
      case App(left, right) =>
        ABT.In[TermF](TermF.App(toScope(left), toScope(right))).scope(Seq.empty)
      case Var(name) =>
        ABT.Var[TermF](Variable.Free(name)).scope(Seq.empty)
    }

  /*def fromABT(exp: ABT[TermF]): Term =
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
    }*/
}