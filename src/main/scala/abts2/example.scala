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

  def toTerm(scope: Scope[TermF]): Term =
    scope.body match {
      case ABT.Var(Variable.Free(name)) => Term.Var(name)
      case ABT.Var(Variable.Bound(name, _)) => Term.Var(name)
      case ABT.In(term) =>
        term match {
          case True() => Term.True
          case False() => Term.False
          case If(guard, thenCase, elseCase) =>
            Term.If(toTerm(guard), toTerm(thenCase), toTerm(elseCase))
          case Abs(body) =>
            assert(scope.names.size == 1)
            Term.Abs(scope.names.head, toTerm(body))
          case App(left, right) =>
            Term.App(toTerm(left), toTerm(right))
        }
    }

  /*def isValue(context: Seq[(String, ABT[TermF])], abt: ABT[TermF]): Boolean =
    abt match {
      case ABT.In[TermF](context, term) =>
        term match {
          case True() => true
          case False() => true
          case Abs(_) => true
          case _ => false
        }
      case ABT.Var[TermF](Variable.Bound(_, index)) => canLookup(context, index)
      case _ => false
    }*/

  @scala.annotation.tailrec
  def lookup[A](context: Seq[A], index: Int): Option[A] =
    if (index == 0) context.headOption
    else lookup(context.tail, index - 1)

  def canLookup[A](context: Seq[A], index: Int): Boolean =
    index < context.size

  def smallStep(context: Seq[(String, ABT[TermF])], abt: ABT[TermF]): Option[ABT[TermF]] =
    abt match {
      case ABT.In(term) =>
        term match {
          case If(guard, thenCase, elseCase) => ???
          case _ => ???
        }
      case ABT.Var(variable) =>
        variable match {
          case Variable.Bound(_, index) => lookup(context, index).map { _._2 }
          case _ => None
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
sealed trait Term {
  import Term._

  def toScope: Scope[TermF] =
    this match {
      case True =>
        ABT.In[TermF](TermF.True()).scope(Seq.empty)
      case False =>
        ABT.In[TermF](TermF.False()).scope(Seq.empty)
      case If(guard, thenCase, elseCase) =>
        ABT.In[TermF](TermF.If(guard.toScope, thenCase.toScope, elseCase.toScope)).scope(Seq.empty)
      case Abs(name, body) =>
        ABT.In[TermF](TermF.Abs(body.toScope)).scope(Seq(name))
      case App(left, right) =>
        ABT.In[TermF](TermF.App(left.toScope, right.toScope)).scope(Seq.empty)
      case Var(name) =>
        ABT.Var[TermF](Variable.Free(name)).scope(Seq.empty)
    }
}

object Term {
  case object True extends Term
  case object False extends Term
  case class If(guard: Term, thenCase: Term, elseCase: Term) extends Term
  case class Abs(name: String, body: Term) extends Term
  case class App(left: Term, right: Term) extends Term
  case class Var(name: String) extends Term
}

/** Used only for testing correctness of Scopes. */
object Manual {
  import Term._

  def freeVars(term: Term): Seq[String] =
    term match {
      case True => Seq.empty
      case False => Seq.empty
      case If(guard, thenCase, elseCase) => (freeVars(guard) ++ freeVars(thenCase) ++ freeVars(elseCase)).distinct
      case Abs(name, body) => freeVars(body).filterNot { _ == name }
      case App(left, right) => (freeVars(left) ++ freeVars(right)).distinct
      case Var(name) => Seq(name)
    }
}
