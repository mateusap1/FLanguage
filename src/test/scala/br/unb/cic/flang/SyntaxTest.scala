package br.unb.cic.flang

import org.scalatest._
import flatspec._
import matchers._

import Syntax._

class SyntaxTest extends AnyFlatSpec with should.Matchers {

  "item parse(\"hello\")" should "return Some(('h', \"ello\"))" in {
    val res = item.parse("hello")
    res should be (Some(('h', "ello")))
  }

  "item parse(\"\")" should "return None" in {
    val res = item.parse("")
    res should be (None)
  }
}
