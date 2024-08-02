package br.unb.cic.flang

import org.scalatest._
import flatspec._
import matchers._

import Interpreter._
import Declarations._
import StateOrErrorMonad._
import cats.data.State

class InterpreterTest extends AnyFlatSpec with should.Matchers {

  val inc = FDeclaration("inc", "x", Add(Id("x"), CTerm(TInt(1))))
  val bug = FDeclaration("bug", "x", Add(Id("y"), CTerm(TInt(1))))

  val declarations = List(inc, bug)

  val initialState: StateData = List()

  "eval CTerm(5)" should "return an integer value 5." in {
    val c5 = CTerm(TInt(5))
    val res = eval(c5, declarations).runA(initialState)
    res should be (Right(TInt(5)))
  }

  "eval Add(CTerm(5), CTerm(10)) " should "return an integer value 15." in {
    val c5  = CTerm(TInt(5))
    val c10 = CTerm(TInt(10))
    val add = Add(c5, c10)
    val res = eval(add, declarations).runA(initialState)
    res should be (Right(TInt(15)))
  }

  "eval Add(CTerm(5), Add(CTerm(5), CTerm(10))) " should "return an integer value 20." in {
    val c5 = CTerm(TInt(5))
    val c10 = CTerm(TInt(10))
    val add = Add(c5, Add(c5, c10))
    val res = eval(add, declarations).runA(initialState)
    res should be(Right(TInt(20)))
  }

  "eval Mul(CTerm(5), CTerm(10))" should "return an integer value 50" in {
    val c5 = CTerm(TInt(5))
    val c10 = CTerm(TInt(10))
    val mul = Mul(c5, CTerm(TInt(10)))
    val res = eval(mul, declarations).runA(initialState)
    res should be(Right(TInt(50)))
  }

  "eval App(inc, 99) " should "return an integer value 100" in {
    val app = App("inc", CTerm(TInt(99)))
    val res = eval(app, declarations).runA(initialState)
    res should be (Right(TInt(100)))
  }

  "eval App(foo, 10) " should "raise an error." in {
    val app = App("foo", CTerm(TInt(10)))
    val res = eval(app, declarations).runA(initialState)
    res should be (Left("Function foo is not declared"))
  }

  "eval Add(5, App(bug, 10)) " should "raise an error." in {
    val c5  = CTerm(TInt(5))
    val app = App("bug", CTerm(TInt(10)))
    val add = Add(c5, app)
    val res = eval(app, declarations).runA(initialState)
    res should be (Left("Variable y not found"))
  }

  "eval IfThenElse(CBool(true), CTerm(1), CTerm(2))" should "return an integer value 1" in {
    val ctrue = CTerm(TBool(true))
    val c1 = CTerm(TInt(1))
    val c2 = CTerm(TInt(2))
    val ifthenelse = IfThenElse(ctrue, c1, c2)
    val res = eval(ifthenelse, declarations).runA(initialState)
    res should be(Right(TInt(1)))
  }

  "eval IfThenElse(CBool(false), CTerm(1), CTerm(2))" should "return an integer value 2" in {
    val cfalse = CTerm(TBool(false))
    val c1 = CTerm(TInt(1))
    val c2 = CTerm(TInt(2))
    val ifthenelse = IfThenElse(cfalse, c1, c2)
    val res = eval(ifthenelse, declarations).runA(initialState)
    res should be(Right(TInt(2)))
  }

  "eval IfThenElse(CTerm(0), CTerm(1), CTerm(2))" should "raise an error." in {
    val c0 = CTerm(TInt(0))
    val c1 = CTerm(TInt(1))
    val c2 = CTerm(TInt(2))
    val ifthenelse = IfThenElse(c0, c1, c2)
    val res = eval(ifthenelse, declarations).runA(initialState)
    res should be(Left("Type Error: Failed to parse boolean"))
  }
}
