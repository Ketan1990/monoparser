package core

case class Parser[A](run: String => List[(A, String)]) {
  self =>

  import Parser._

  def map[B](f: A => B): Parser[B] = self.flatMap(a => result(f(a)))

  def flatMap[B](f: A => Parser[B]): Parser[B] = bind(self)(f)

  def plus: Parser[A] => Parser[A] =  p2 => {
    Parser(inp => self.run(inp) ++: p2.run(inp))
  }


}

object Parser {
  def result[A]: A => Parser[A] = a => Parser(input => List((a, input)))

  def zero[A]: Parser[A] = Parser(_ => List())

  def item: Parser[Char] = Parser(inp =>
    inp.toList match {
      case Nil => Nil
      case x :: xs => List((x, xs.mkString))
    })

  def seq[A, B]: Parser[A] => Parser[B] => Parser[(A, B)] = p => q => {
    for {a <- p; b <- q} yield (a, b)
  }

  def bind[A, B](a: Parser[A])(f: A => Parser[B]): Parser[B] =
    Parser { input =>
      (for {(v, inp) <- a.run(input)} yield f(v).run(inp)).flatten
    }

  def sat(p: Char => Boolean): Parser[Char] = item.flatMap(a => if (p(a)) result(a) else zero)

  /*  Using sat, we can define parsers for specific characters, single digits, lower-case
    letters, and upper-case letters:*/
  def char: Char => Parser[Char] = x => {
    sat(y => x == y)
  }

  def digit: Parser[Char] = sat(x => '0' <= x && x <= '9')

  def lower: Parser[Char] = sat(x => 'a' <= x && x <= 'z')

  def upper: Parser[Char] = sat(x => 'A' <= x && x <= 'Z')

  def letter:Parser[Char] = lower.plus(upper)

  def alphaNum:Parser[Char] = letter plus digit

  def word: Parser[String] = {

    val newWord: Parser[String] = for {
      x <- letter
      y <- word
    }yield x.toString.concat(y)

    newWord plus result("")

  }
}