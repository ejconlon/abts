package abts

import org.scalatest.FunSpec

class ABTSpec extends FunSpec {
  describe("ABT") {
    it("works") {
      val a = Term.App(Term.Abs("x", Term.Var("x")), Term.Var("y"))
      val b = Term.toABT(a)
      assert(ABT.freeVars(b) == Set("y"))
      val c = Term.fromABT(b)
      assert(c == a)
      val d = Term.bigStep(b)
      val e = Term.fromABT(d)
      assert(e == Term.Var("y"))
    }
  }
}