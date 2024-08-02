package br.unb.cic.flang

import StateOrErrorMonad._

sealed trait Term

case class TInt(v: Integer) extends Term
case class TBool(b: Boolean) extends Term

object Term {
  def parseInt(t: Term): ErrorOr[Integer] = {
    t match {
      case TInt(v) => Right(v)
      case _ => Left("Type Error: Failed to parse integer")
    }
  }

  def parseBool(t: Term): ErrorOr[Boolean] = {
    t match {
      case TBool(v) => Right(v)
      case _ => Left("Type Error: Failed to parse boolean")
    }
  }
}