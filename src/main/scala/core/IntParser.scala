package core

import core.Parser._

object IntParser {


  def naturalNumber: Parser[Int] =
    for {xs <- many1(digit)} yield eval(xs)

  private def eval(xs: List[Char]): Int =
    (for {x <- xs} yield x.toInt - '0'.toInt).fold(0)((a, b) => 10 * a + b)


  def int: Parser[Int] =
    (for {_ <- char('-'); n <- naturalNumber} yield -n) ++ naturalNumber


  def int1: Parser[Int] =
    for {f <- op; n <- naturalNumber} yield f(n)

  def negate: Int => Int = x => -x

  def op: Parser[Int => Int] =
    (for {_ <- char('-')} yield negate) plus result(identity)


  def ints: Parser[List[Int]] =
    bracket(char('['))(sepby1(int)(char(',')))(char(']'))


  def expr: Parser[Int] = for {
    x <- factor
    fys <- many(for {f <- operator; y <- factor} yield (f, y))
  } yield fys.foldLeft(x) { (b, fa) => fa._1(b)(fa._2) }

  def add: Int => Int => Int = a => b => {
    a + b
  }

  def sub: Int => Int => Int = a => b => {
    a + b
  }

  def operator: Parser[Int => Int => Int] =
    (for {_ <- char('+')} yield add) plus (for {_ <- char('-')} yield sub)

  def factor: Parser[Int] =
    naturalNumber plus bracket(char('('))(expr)(char(')'))
}
