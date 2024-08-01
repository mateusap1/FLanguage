package br.unb.cic.flang

package object Syntax {
  case class Parser[A](parse: (String => Option[(A, String)])) {
    def map[B](f: A => B): Parser[B] = {
      return Parser[B](
        (
            cs =>
              parse(cs) match {
                case None          => None
                case Some((c, cs)) => Some((f(c), cs))
              }
        )
      )
    }

    def flatMap[B](f: A => Parser[B]): Parser[B] = {
      return Parser[B](
        (
            cs =>
              parse(cs) match {
                case None           => None
                case Some((x, cs2)) => f(x).parse(cs2)
              }
        )
      )
    }

    def +++(q: Parser[A]): Parser[A] = {
      Parser[A](cs =>
        this.parse(cs) match {
          case None =>
            q.parse(cs) match {
              case None => None
              case r    => r
            }
          case r => r
        }
      )
    }
  }

  object Parser {
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
  }

  val item = Parser[Char](
    (
        cs =>
          cs match {
            case ""  => None
            case str => Some((str.head, str.tail))
          }
    )
  )

  def failure[A]() = {
    Parser[A](_ => None)
  }

  def fun(pr: (Char => Boolean))(c: Char): Parser[Char] = {
    if (pr(c)) {
      Parser.pure(c)
    } else {
      Parser(cs2 => None)
    }
  }

  def sat(pr: (Char => Boolean)): Parser[Char] = {
    for {
      c <- item
      y <- if (pr(c)) Parser.pure(c) else failure
    } yield y
  }

  def char(c: Char): Parser[Char] = {
    sat(c2 => c == c2)
  }

  def string(str: String): Parser[String] = {
    str match {
      case "" => Parser.pure("")
      case s => for {
        _ <- char (s.head)
        _ <- string (s.tail)
      } yield s
    }
  }

  def many[A](p: Parser[A]): Parser[List[A]] = {
    many1(p) +++ Parser.pure(List())
  }

  def many1[A](p: Parser[A]): Parser[List[A]] = {
    for {
      a <- p
      as <- many(p)
    } yield a :: as
  }

  
}
