package br.unb.cic.flang

sealed trait Expr

case class CTerm(v: Term) extends Expr
case class Add(lhs: Expr, rhs: Expr) extends Expr
case class Mul(lhs: Expr, rhs: Expr) extends Expr
case class IfThenElse(pr: Expr, thenBranch: Expr, elseBranch: Expr) extends Expr

case class Id(name: String) extends Expr
case class App(name: String, arg: Expr) extends Expr
