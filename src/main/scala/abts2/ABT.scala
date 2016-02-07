package abts2

import prelude._

// https://github.com/psygnisfive/SimpleFP-v2/blob/master/src/Utils/ABT.hs

sealed trait Variable {
  import Variable.{Free, Bound, Meta}

  def name: String
  def equals(other: Variable): Boolean =
    (this, other) match {
      case (Free(x), Free(y)) => x == y
      case (Bound(_, i), Bound(_, j)) => i == j
      case (Meta(i), Meta(j)) => i == j
      case _ => false
    }
}

object Variable {
  case class Free(name: String) extends Variable
  case class Bound(name: String, index: Int) extends Variable
  case class Meta(number: Int) extends Variable {
    override def name = "?" + number
  }
}

sealed trait ABT[F[_]]

object ABT {
  case class Var[F[_]](variable: Variable) extends ABT[F]
  case class In[F[_]](term: F[Scope[F]]) extends ABT[F]
}

case class Scope[F[_]](names: Seq[String], freeNames: Seq[Variable.Free], body: ABT[F])
