package abts

import org.scalatest.FunSpec

class ABTSpec extends FunSpec {
  describe("ABT") {
    it("works") {
      val a = Term.App(Term.Abs("x", Term.Var("x")), Term.Var("y"))
      val b = Term.toABT(a)
      assert(ABT.freeVars(b) == Set("y"))
      val c = Term.fromABT(b)
      assert(c === a)
      val d = Term.bigStep(b)
      val e = Term.fromABT(d)
      assert(e === Term.Var("y"))
    }

    it("doesn't sub bound variables") {
      val orig = Term.Abs("x", Term.Var("x"))
      val actual = Term.fromABT(ABT.substitute("x", ABT.Var("z"), Term.toABT(orig)))
      val expected = orig
      assert(actual === expected)
    }

    /*it("subs free variables") {
      val orig = Term.Abs("x", Term.Var("y"))
      val actual = Term.fromABT(ABT.substitute("y", ABT.Var("z"), Term.toABT(orig)))
      val expected = Term.Abs("x", Term.Var("z"))
      assert(actual === expected)
    }*/
  }
}