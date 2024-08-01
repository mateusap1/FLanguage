package br.unb.cic.flang

import cats.Monad

package object Syntax {
  case class Parser[A](parse: (String => Option[(A, String)]))
  val ParserMonad = new Monad[Parser] {
    def pure[A](x: A): Parser[A] = {
      return Parser[A]((cs => Some((x, cs))))
    }
    def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B] = {
      return Parser[B](
        (
            cs =>
              p.parse(cs) match {
                case None           => None
                case Some((x, cs2)) => f(x).parse(cs2)
              }
        )
      )
    }
    def tailRecM[A, B](a: A)(f: A => Parser[Either[A, B]]): Parser[B] = ???
  }
  // object Parser extends Monad[Parser] {
  //   def pure[A](x: A): Parser[A] = {
  //     return Parser[A]((cs => Some((x, cs))))
  //   }
  //   def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B] = {
  //     return Parser[B](
  //       (
  //           cs =>
  //             p.parse(cs) match {
  //               case None           => None
  //               case Some((x, cs2)) => f(x).parse(cs2)
  //             }
  //       )
  //     )
  //   }
  //   def tailRecM[A, B](a: A)(f: A => Parser[Either[A, B]]): Parser[B] = ???
  // }

  val item = Parser[Char](
    (
        cs =>
          cs match {
            case ""  => None
            case str => Some((str.charAt(0), str.slice(1, str.length)))
          }
    )
  )

  def +++[A](p: Parser[A])(q: Parser[A]) {
    Parser[A](cs =>
      p.parse(cs) match {
        case None =>
          q.parse(cs) match {
            case None => None
            case r    => r
          }
        case r => r
      }
    )
  }

  def fun(pr: (Char => Boolean))(c: Char): Parser[Char] = {
    if (pr(c)) {
      ParserMonad.pure(c)
    } else {
      Parser(cs2 => None)
    }
  }

  def sat(pr: (Char => Boolean)): Parser[Char] = {
    ParserMonad.flatMap(item)(fun(pr))
  }
}
