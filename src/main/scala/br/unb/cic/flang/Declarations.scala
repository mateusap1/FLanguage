package br.unb.cic.flang

import StateOrErrorMonad._

case class FDeclaration(name: String, arg: String, body: Expr)

object Declarations {

  def lookup(
      name: String,
      declarations: List[FDeclaration]
  ): ErrorOr[FDeclaration] = declarations match {
    case List() => Left(s"Function $name is not declared")
    case ((f @ FDeclaration(n, a, b)) :: _) if n == name => Right(f)
    case (_ :: fs)                                       => lookup(name, fs)
  }

}
