package br.unb.cic.flang

import Declarations._
import StateOrErrorMonad._
import cats.data.State
import cats.data.StateT
import javax.naming.NameNotFoundException

object Interpreter {

  /** This implementation relies on a state monad.
    *
    * Here we replace the substitution function (that needs to traverse the AST
    * twice during interpretation), by a 'global' state that contains the
    * current 'bindings'. The bindings are pairs from names to integers.
    *
    * We only update the state when we are interpreting a function application.
    * This implementation deals with sections 6.1 and 6.2 of the book
    * "Programming Languages: Application and Interpretation". However, here we
    * use a monad state, instead of passing the state explicitly as an agument
    * to the eval function.
    *
    * Sections 6.3 and 6.4 improves this implementation. We will left such an
    * improvements as an exercise.
    */
  def eval(
      expr: Expr,
      declarations: List[FDeclaration]
  ): StateOrError[Integer] =
    expr match {
      case CInt(v) => pure(v)

      case Add(lhs, rhs) =>
        for {
          l <- eval(lhs, declarations)
          r <- eval(rhs, declarations)
        } yield l + r
      case Mul(lhs, rhs) =>
        for {
          l <- eval(lhs, declarations)
          r <- eval(rhs, declarations)
        } yield l * r
      case Id(name) =>
        for {
          state <- get
          result <- StateT[ErrorOr, StateData, Integer](s =>
            lookupVar(name, state) match {
              case Left(err) => Left(err)
              case Right(n)  => Right((s, n))
            }
          )
        } yield result
      case App(name, arg) => {
        val fdecl = lookup(name, declarations)
        fdecl match {
          case Left(err) => assertError(err)
          case Right(fdeclR) =>
            for {
              value <- eval(arg, declarations)
              s1 <- get
              s2 <- set(declareVar(fdeclR.arg, value, s1))
              result <- eval(fdeclR.body, declarations)
            } yield result
        }
      }
    }
}
