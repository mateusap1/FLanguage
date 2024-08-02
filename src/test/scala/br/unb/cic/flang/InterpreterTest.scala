package br.unb.cic.flang

import org.scalatest._
import flatspec._
import matchers._

import Interpreter._
import Declarations._
import StateOrErrorMonad._
import cats.data.State

class InterpreterTest extends AnyFlatSpec with should.Matchers {

  val inc = FDeclaration("inc", "x", Add(Id("x"), CInt(TInt(1))))
  val bug = FDeclaration("bug", "x", Add(Id("y"), CInt(TInt(1))))

  val declarations = List(inc, bug)

  val initialState: StateData = List()

  "eval CInt(5)" should "return an integer value 5." in {
    val c5 = CInt(TInt(5))
    val res = eval(c5, declarations).runA(initialState)
    res should be (Right(TInt(5)))
  }

  "eval Add(CInt(5), CInt(10)) " should "return an integer value 15." in {
    val c5  = CInt(TInt(5))
    val c10 = CInt(TInt(10))
    val add = Add(c5, c10)
    val res = eval(add, declarations).runA(initialState)
    res should be (Right(TInt(15)))
  }

  "eval Add(CInt(5), Add(CInt(5), CInt(10))) " should "return an integer value 20." in {
    val c5 = CInt(TInt(5))
    val c10 = CInt(TInt(10))
    val add = Add(c5, Add(c5, c10))
    val res = eval(add, declarations).runA(initialState)
    res should be(Right(TInt(20)))
  }

  "eval Mul(CInt(5), CInt(10))" should "return an integer value 50" in {
    val c5 = CInt(TInt(5))
    val c10 = CInt(TInt(10))
    val mul = Mul(c5, CInt(TInt(10)))
    val res = eval(mul, declarations).runA(initialState)
    res should be(Right(TInt(50)))
  }

  "eval App(inc, 99) " should "return an integer value 100" in {
    val app = App("inc", CInt(TInt(99)))
    val res = eval(app, declarations).runA(initialState)
    res should be (Right(TInt(100)))
  }

  "eval App(foo, 10) " should "raise an error." in {
    val app = App("foo", CInt(TInt(10)))
    val res = eval(app, declarations).runA(initialState)
    res should be (Left("Function foo is not declared"))
  }

  "eval Add(5, App(bug, 10)) " should "raise an error." in {
    val c5  = CInt(TInt(5))
    val app = App("bug", CInt(TInt(10)))
    val add = Add(c5, app)
    val res = eval(app, declarations).runA(initialState)
    res should be (Left("Variable y not found"))
  }

  "eval IfThenElse(CBool(true), CInt(1), CInt(2))" should "return an integer value 1" in {
    val ctrue = CInt(TBool(true))
    val c1 = CInt(TInt(1))
    val c2 = CInt(TInt(2))
    val ifthenelse = IfThenElse(ctrue, c1, c2)
    val res = eval(ifthenelse, declarations).runA(initialState)
    res should be(Right(TInt(1)))
  }
}
