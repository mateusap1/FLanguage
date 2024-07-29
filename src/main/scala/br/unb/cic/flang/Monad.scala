package br.unb.cic.flang
// import cats.Monad
import cats.data.State

package object StateMonad {
  type StateData = List[(String, Integer)]

  def declareVar(name: String, value: Integer, state: StateData): StateData =
    (name, value) :: state

  def lookupVar(name: String, state: StateData): Integer = state match {
    case List()                      => ???
    case (n, v) :: tail if n == name => v
    case _ :: tail                   => lookupVar(name, tail)
  }
}
