package br.unb.cic.flang
// import cats.Monad
import cats.data.State
import javax.naming.NameNotFoundException
import cats.MonadError
import cats.data.StateT
import cats.data.EitherT
import java.util.concurrent.Future
import javax.naming.InvalidNameException

package object StateMonad {
  type StateData = List[(String, Integer)]
  type ErrorOr[A] = Either[String, A]

  type StateOrError[A] = StateT[ErrorOr, StateData, A]

  def pure[A](element: A): StateOrError[A] = StateT.pure(element)

  def get: StateOrError[StateData] = StateT.get

  def set[A](state: StateData): StateOrError[Unit] = StateT.set(state)

  def valueF[A](value: ErrorOr[A]): A = value match {
    case Left(value) => throw new NameNotFoundException(value)
    case Right(value) => value
  } 

  def declareVar(name: String, value: Integer, state: StateData): StateData = 
    (name, value) :: state
  

  def lookupVar(name: String, state: StateData): ErrorOr[Integer] = state match {
    case List()                      => Left(s"Variable $name not found")
    case (n, v) :: tail if n == name => Right(v)
    case _ :: tail                   => lookupVar(name, tail)  
  }     
}
