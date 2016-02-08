package abts2

import org.scalatest.FunSuite
import org.scalatest.prop.Checkers
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen
import org.scalacheck.Prop.forAll

object ABTSpec {
  import org.scalatest.Assertions._
  import Term._

  val termGen: Gen[Term] = Gen.sized { sizedTermGen(_) }

  private[this] val genName: Gen[String] =
    Gen.identifier

  private[this] def sizedTermGen(size: Int): Gen[Term] =
    if (size == 0) Gen.oneOf(Gen.const(True), Gen.const(False), varGen)
    else Gen.oneOf(ifGen(size), appGen(size), absGen(size))

  private[this] def ifGen(size: Int): Gen[Term] =
    for {
      gs <- Gen.chooseNum(0, size - 1)
      g <- sizedTermGen(gs)
      ts <- Gen.chooseNum(0, size - 1)
      t <- sizedTermGen(ts)
      es <- Gen.chooseNum(0, size - 1)
      e <- sizedTermGen(es)
    } yield If(g, t, e)

  private[this] def appGen(size: Int): Gen[Term] =
    for {
      ls <- Gen.chooseNum(0, size - 1)
      l <- sizedTermGen(ls)
      rs <- Gen.chooseNum(0, size - 1)
      r <- sizedTermGen(rs)
    } yield App(l, r)

  private[this] def absGen(size: Int): Gen[Term] =
    for {
      n <- genName
      bs <- Gen.chooseNum(0, size - 1)
      b <- sizedTermGen(bs)
    } yield Abs(n, b)

  private[this] val varGen: Gen[Term] =
    for {
      n <- genName
    } yield Var(n)

  def assertFreeVars(term: Term, expected: Seq[String]) {
    val manualActual = Manual.freeVars(term)
    assert(manualActual == expected, "manual")
    val scope = term.toScope
    val scopeActual = scope.freeNames.map { _.name }
    assert(scopeActual == expected, "scope: " + scope)
    val termAgain = Term.fromScope(scope)
    assert(termAgain == term)
  }
}

class ABTSpec extends FunSuite with Checkers {
  import ABTSpec._
  import Term._

  private[this] val minSuccessful = 1000

  implicit override val generatorDrivenConfig =
    PropertyCheckConfig(minSuccessful = minSuccessful, maxDiscarded = 5 * minSuccessful)

  test("calculate free vars (1)") {
    assertFreeVars(App(Abs("x", Var("x")), Var("y")), Seq("y"))
  }

  test("calculate free vars (2)") {
    assertFreeVars(If(App(Var("x"), True), Abs("y", True), Abs("z" ,True)), Seq("x"))
  }

  test("calculate free vars (3)") {
    assertFreeVars(App(Abs("a",If(False,Var("b"),True)),App(If(False,Var("c"),False),Abs("d",False))), Seq("b", "c"))
  }

  test("calculate free vars (4)") {
    assertFreeVars(If(If(False,Var("h"),Var("h")),Abs("pxO",True),Var("gah")), Seq("h", "gah"))
  }

  test("calculate free vars with gens") {
    check {
      forAll(termGen) { term =>
        val expected = Manual.freeVars(term)
        val scope = term.toScope
        val actual = scope.freeNames.map { _.name }
        val termAgain = Term.fromScope(scope)
        (actual == expected) && (termAgain == term)
      }
    }
  }

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