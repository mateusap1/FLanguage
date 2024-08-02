package br.unb.cic.flang

import org.scalatest._
import flatspec._
import matchers._

import Syntax._

class SyntaxTest extends AnyFlatSpec with should.Matchers {

  "item parse(\"hello\")" should "return Some(('h', \"ello\"))" in {
    val res = item parse("hello")
    res should be (Some(('h', "ello")))
  }

  "item parse(\"\")" should "return None" in {
    val res = item parse("")
    res should be (None)
  }

  "(failure() +++ item) parse(\"hello\")" should "return Some(('h', \"ello\"))" in {
    val parser = failure() +++ item
    val res = parser parse("hello")
    res should be (Some(('h', "ello")))
  }

  "(item +++ failure()) parse(\"hello\")" should "return Some(('h', \"ello\"))" in {
    val parser = item +++ failure
    val res = parser parse("hello")
    res should be (Some(('h', "ello")))
  }

  "(many string(\"foo\")) parse(\"foobarfoobar\")" should "return Some((List(\"foo\"), \"barfoobar\"))" in {
    val parser = many(string("foo"))
    val res = parser parse("foobarfoobar")
    res should be (Some((List("foo"), "barfoobar")))
  }

  "(many string(\"foo\")) parse(\"foofoobar\")" should "return Some((List(\"foo\", \"foo\"), \"bar\"))" in {
    val parser = many(string("foo"))
    val res = parser parse("foofoobar")
    res should be (Some((List("foo", "foo"), "bar")))
  }

  "expr parse(\"1 + 2\")" should "return Some(Add(CInt(1), CInt(2)), \"\")" in {
    val res = apply(expr)("1 + 2")
    res should be (Some((Add(CInt(TInt(1)), CInt(TInt(2))), "")))
  }

  "expr parse(\" 2 * 3 + 4 \")" should "return Some((Add(Mul(CInt(2), CInt(3)), CInt(4)), \"\")" in {
    val res = apply(expr)(" 2 * 3 + 4 ")
    res should be (Some((Add(Mul(CInt(TInt(2)), CInt(TInt(3))), CInt(TInt(4))), "")))
  }
}
