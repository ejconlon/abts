package abts2

import org.scalatest.FunSuite
import org.scalatest.prop.Checkers
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen
import org.scalacheck.Prop.forAll

object ABTSpec {
  import org.scalatest.Assertions._
  import Term._

  def termGen: Gen[Term] = Gen.sized { sizedTermGen(_) }

  private[this] def sizedTermGen(size: Int): Gen[Term] =
    if (size == 0) Gen.oneOf(Gen.const(True), Gen.const(False), varGen)
    else Gen.oneOf(ifGen(size), appGen(size), absGen(size))

  private[this] def ifGen(size: Int): Gen[Term] =
    for {
      g <- sizedTermGen(size - 1)
      t <- sizedTermGen(size - 1)
      e <- sizedTermGen(size - 1)
    } yield If(g, t, e)

  private[this] def appGen(size: Int): Gen[Term] =
    for {
      l <- sizedTermGen(size - 1)
      r <- sizedTermGen(size - 1)
    } yield App(l, r)

  private[this] def absGen(size: Int): Gen[Term] =
    for {
      n <- arbitrary[String]
      b <- sizedTermGen(size - 1)
    } yield Abs(n, b)

  private[this] def varGen: Gen[Term] =
    for {
      n <- arbitrary[String]
    } yield Var(n)

  def assertFreeVars(term: Term, expected: Seq[String]) {
    val manualActual = Manual.freeVars(term)
    assert(manualActual == expected, "manual")
    val scope = toScope(term)
    val scopeActual = scope.freeNames.map { _.name }
    assert(scopeActual == expected, "scope: " + scope)
  }
}

class ABTSpec extends FunSuite with Checkers {
  import ABTSpec._
  import Term._

  test("calculate free vars (1)") {
    assertFreeVars(App(Abs("x", Var("x")), Var("y")), Seq("y"))
  }

  test("calculate free vars (2)") {
    assertFreeVars(If(App(Var("x"), True), Abs("y", True), Abs("z" ,True)), Seq("x"))
  }

  test("calculate free vars (3)") {
    assertFreeVars(App(Abs("a",If(False,Var("b"),True)),App(If(False,Var("c"),False),Abs("d",False))), Seq("b", "c"))
  }

  /*test("calculate free vars with gens") {
    check {
      forAll(termGen) { term =>
        val expected = Manual.freeVars(term)
        val scope = toScope(term)
        val actual = scope.freeNames.map { _.name }
        actual == expected
      }
    }
  }*/

  /*test("works") {
    val a = Term.App(Term.Abs("x", Term.Var("x")), Term.Var("y"))
    val b = Term.toABT(a)
    assert(ABT.freeVars(b) == Set("y"))
    val c = Term.fromABT(b)
    assert(c === a)
    val d = Term.bigStep(b)
    val e = Term.fromABT(d)
    assert(e === Term.Var("y"))
  }*/

  /*test("doesn't sub bound variables") {
    val orig = Term.Abs("x", Term.Var("x"))
    val actual = Term.fromABT(ABT.substitute("x", ABT.Var("z"), Term.toABT(orig)))
    val expected = orig
    assert(actual === expected)
  }*/

  /*test("subs free variables") {
    val orig = Term.Abs("x", Term.Var("y"))
    val actual = Term.fromABT(ABT.substitute("y", ABT.Var("z"), Term.toABT(orig)))
    val expected = Term.Abs("x", Term.Var("z"))
    assert(actual === expected)
  }*/
}