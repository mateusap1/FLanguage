package br.unb.cic.flang

import org.scalatest._
import flatspec._
import matchers._

import Syntax._

class SyntaxTest extends AnyFlatSpec with should.Matchers {

  "item parse(\"hello\")" should "return Some(('h', \"ello\"))" in {
    val res = item parse ("hello")
    res should be(Some(('h', "ello")))
  }

  "item parse(\"\")" should "return None" in {
    val res = item parse ("")
    res should be(None)
  }

  "(failure() +++ item) parse(\"hello\")" should "return Some(('h', \"ello\"))" in {
    val parser = failure() +++ item
    val res = parser parse ("hello")
    res should be(Some(('h', "ello")))
  }

  "(item +++ failure()) parse(\"hello\")" should "return Some(('h', \"ello\"))" in {
    val parser = item +++ failure
    val res = parser parse ("hello")
    res should be(Some(('h', "ello")))
  }

  "(many string(\"foo\")) parse(\"foobarfoobar\")" should "return Some((List(\"foo\"), \"barfoobar\"))" in {
    val parser = many(string("foo"))
    val res = parser parse ("foobarfoobar")
    res should be(Some((List("foo"), "barfoobar")))
  }

  "(many string(\"foo\")) parse(\"foofoobar\")" should "return Some((List(\"foo\", \"foo\"), \"bar\"))" in {
    val parser = many(string("foo"))
    val res = parser parse ("foofoobar")
    res should be(Some((List("foo", "foo"), "bar")))
  }

  "alpha parse(\"blah blah blah\")" should "return Some((\"blah\", \"blah blah\"))" in {
    val res = alpha parse ("blah blah blah")
    res should be(Some(("blah", "blah blah")))
  }

  "expr parse(\" 1 \")" should "return Some(1), \"\")" in {
    val res = apply(expr)(" 1 ")
    res should be(Some(CTerm(TInt(1)), ""))
  }

  "expr parse(\"1 + 2\")" should "return Some(Add(1, 2), \"\")" in {
    val res = apply(expr)("1 + 2")
    res should be(Some((Add(CTerm(TInt(1)), CTerm(TInt(2))), "")))
  }

  "expr parse(\"12 + 23\")" should "return Some(Add(12, 23), \"\")" in {
    val res = apply(expr)("12 + 23")
    res should be(Some((Add(CTerm(TInt(12)), CTerm(TInt(23))), "")))
  }

  "expr parse(\" 2 * 3 + 4 \")" should "return Some((Add(Mul(2, 3), 4), \"\")" in {
    val res = apply(expr)(" 2 * 3 + 4 ")
    res should be(
      Some((Add(Mul(CTerm(TInt(2)), CTerm(TInt(3))), CTerm(TInt(4))), ""))
    )
  }

  "expr parse(\" 2 * (3 + 4) \")" should "return Some((Mul(2, Add(3, 4)), \"\")" in {
    val res = apply(expr)(" 2 * (3 + 4) ")
    val c2 = CTerm(TInt(2))
    val c3 = CTerm(TInt(3))
    val c4 = CTerm(TInt(4))

    res should be(Some((Mul(c2, Add(c3, c4)), "")))
  }

  "expr parse(\" 2 * (x + 4) \")" should "return Some((Mul(2, Add(x, 4)), \"\")" in {
    val res = apply(expr)(" 2 * (x + 4) ")
    val c2 = CTerm(TInt(2))
    val cx = Id("x")
    val c4 = CTerm(TInt(4))

    res should be(Some((Mul(c2, Add(cx, c4)), "")))
  }

  "expr parse(\" 2 * cf(true) \")" should "return Some(Mul(2, App(cf, true)), \"\")" in {
    val res = apply(expr)(" 2 * cf(true) ")
    val c2 = CTerm(TInt(2))
    val cf = App("cf", CTerm(TBool(true)))
    val exp = Mul(c2, cf)

    res should be(Some((exp, "")))
  }

  "expr parse(\"if true then 1 else 4\")" should "return Some(IfThenElse(true, 1, 4), \"\")" in {
    val res = apply(expr)("if true then 1 else 4")
    val ctrue = CTerm(TBool(true))
    val c1 = CTerm(TInt(1))
    val c4 = CTerm(TInt(4))
    res should be(Some(IfThenElse(ctrue, c1, c4), ""))
  }

  "expr parse(\"if x then (1 + 2) else 4 * 6 + 1\")" should "return Some(IfThenElse(x, 1 + 2, 4 * 6 + 1), \"\")" in {
    val res = apply(expr)("if x then (1 + 2) else 4 * 6 + 1")
    val cx = Id("x")
    val expr1 = Add(CTerm(TInt(1)), CTerm(TInt(2)))
    val expr2 = Add(Mul(CTerm(TInt(4)), CTerm(TInt(6))), CTerm(TInt(1)))
    res should be(Some(IfThenElse(cx, expr1, expr2), ""))
  }

  "fdecl parse(\"func coolfunc(arg)\")" should "return Some(((\"coolfunc\", \"arg\"), \"\"))" in {
    val res = fdecl parse ("func coolfunc(arg)")
    res should be(Some((("coolfunc", "arg"), "")))
  }

  "func parse(\"func coolfunc(arg) = 1 + 3\")" should "return Some((FDeclaration(\"coolfunc\", \"arg\", expr), \"\"))" in {
    val res = func parse ("func coolfunc(arg) = 1 + 3")
    val c1 = CTerm(TInt(1))
    val c3 = CTerm(TInt(3))
    val expr = Add(c1, c3)
    res should be(Some((FDeclaration("coolfunc", "arg", expr), "")))
  }

  "func parse(\"func coolfunc(arg) = 1 + 3\nfunc otherfunc(arg) = 2 * 6\")" should "return Some((FDeclaration(\"coolfunc\", \"arg\", expr), \"\"))" in {
    val res =
      prog parse ("func coolfunc(arg) = 1 + 3\nfunc otherfunc(arg) = 2 * 6")

    val c1 = CTerm(TInt(1))
    val c3 = CTerm(TInt(3))
    val expr1 = Add(c1, c3)

    val c2 = CTerm(TInt(2))
    val c6 = CTerm(TInt(6))
    val expr2 = Mul(c2, c6)

    res should be(
      Some(
        (
          List(
            FDeclaration("coolfunc", "arg", expr1),
            FDeclaration("otherfunc", "arg", expr2)
          ),
          ""
        )
      )
    )
  }
}
