package br.unb.cic.flang
// import cats.Monad
import cats.data.State
import javax.naming.NameNotFoundException
import cats.MonadError
import cats.data.StateT
import cats.data.EitherT
import java.util.concurrent.Future
import javax.naming.InvalidNameException

package object StateOrErrorMonad {
  type StateData = List[(String, Term)]
  type ErrorOr[A] = Either[String, A]

  type StateOrError[A] = StateT[ErrorOr, StateData, A]

  def pure[A](element: A): StateOrError[A] = StateT.pure(element)

  def get: StateOrError[StateData] = StateT.get

  def set[A](state: StateData): StateOrError[Unit] = StateT.set(state)

  def assertError[A](error: String): StateOrError[A] = {
    StateT[ErrorOr, StateData, A](_ => Left(error))
  }

  def assertValue[A](value: A): StateOrError[A] = {
    StateT.pure(value)
  }

  def parseErrorOr[A](value: ErrorOr[A]): StateOrError[A] = {
    value match {
      case Left(err) => assertError(err)
      case Right(v) => assertValue(v)
    }
  }

  def declareVar(name: String, value: Term, state: StateData): StateData =
    (name, value) :: state

  def lookupVar(name: String, state: StateData): ErrorOr[Term] =
    state match {
      case List()                      => Left(s"Variable $name not found")
      case (n, v) :: tail if n == name => Right(v)
      case _ :: tail                   => lookupVar(name, tail)
    }
}
