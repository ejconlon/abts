package abts2

import prelude._

// https://github.com/psygnisfive/SimpleFP-v2/blob/master/src/Utils/ABT.hs

sealed trait Variable
object Variable {
  case class Free(name: String) extends Variable
  case class Bound(index: Int) extends Variable
  //case class Meta(number: Int) extends Variable
}

sealed trait ABT[F[_]]
object ABT {
  case class Var[F[_]](variable: Variable) extends ABT[F]
  case class In[F[_]](term: F[Scope[F]]) extends ABT[F]
}

case class Scope[F[_]](names: Seq[String], freeNames: Seq[Variable.Free], body: ABT[F])
