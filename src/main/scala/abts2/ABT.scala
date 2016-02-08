package abts2

import prelude._

// https://github.com/psygnisfive/SimpleFP-v2/blob/master/src/Utils/ABT.hs

sealed trait Variable {
  import Variable.{Free, Bound} //, Meta}

  def name: String
  def equals(other: Variable): Boolean =
    (this, other) match {
      case (Free(x), Free(y)) => x == y
      case (Bound(_, i), Bound(_, j)) => i == j
      //case (Meta(i), Meta(j)) => i == j
      case _ => false
    }
}

object Variable {
  case class Free(name: String) extends Variable
  case class Bound(name: String, index: Int) extends Variable
  //case class Meta(number: Int) extends Variable {
  //  override def name = "?" + number
  //}
}

sealed trait ABT[F[_]] {
  import ABT.{Var, In}

  def fold[A](
    onVar: Variable => A,
    onRec: F[A] => A,
    onScope: (Int, A) => A
  )(
    implicit functor: Functor[F]
  ): A =
    this match {
      case Var(variable) => onVar(variable)
      case In(term) => onRec(functor.map(term) { _.fold[A](onVar, onRec, onScope) })
    }

  def freeVars(implicit foldable: Foldable[F]): Seq[Variable.Free] =
    fold[Seq[Variable.Free]](
      {
        case Variable.Free(name) => Seq(Variable.Free(name))
        case _ => Seq.empty[Variable.Free]
      },
      { foldable.fold[Seq[Variable.Free]](_) },
      { case (_, fvs) => fvs }
    ).distinct

  def bind(numBinders: Int, fvs: Seq[Variable.Free])(implicit foldable: Foldable[F]): ABT[F] =
    if (fvs.isEmpty)
      this
    else
      this match {
        case Var(fv@Variable.Free(_)) =>
          fvs.zipWithIndex.find { case (x, _) => x == fv } match {
            case None => this
            case Some((fv, i)) => Var(Variable.Bound(fv.name, i + numBinders))
          }
        case In(term) => In(foldable.map(term) { _.bind(numBinders, fvs) })
        case _ => this
      }

  def scope(names: Seq[String])(implicit foldable: Foldable[F]): Scope[F] = {
    val bound: ABT[F] = bind(0, names.map { Variable.Free(_) })
    Scope(names, bound.freeVars, bound)
  }
}

object ABT {
  case class Var[F[_]](variable: Variable) extends ABT[F]
  case class In[F[_]](term: F[Scope[F]]) extends ABT[F]
}

case class Scope[F[_]](names: Seq[String], freeNames: Seq[Variable.Free], body: ABT[F]) {
  def fold[A](
    onVar: Variable => A,
    onRec: F[A] => A,
    onScope: (Int, A) => A
  )(
    implicit functor: Functor[F]
  ): A =
    onScope(names.size, body.fold[A](onVar, onRec, onScope))

  def bind(numBinders: Int, fvs: Seq[Variable.Free])(implicit foldable: Foldable[F]): Scope[F] =
    if (fvs.isEmpty)
      this
    else {
      val newBody = body.bind(numBinders + freeNames.size, fvs)
      val newFreeNames = newBody.freeVars
      Scope(names, newFreeNames, newBody)
    }
}
