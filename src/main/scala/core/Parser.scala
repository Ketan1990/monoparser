package core

case class Parser[A](run: String => List[(A, String)]) {
  self =>

  import Parser._

  def map[B](f: A => B): Parser[B] = self.bind(a => result(f(a)))

  def bind[B](f: A => Parser[B]): Parser[B] =
    Parser(input =>
      (for {p <- self.run(input)} yield f(p._1)).flatten
    )
}

object Parser {
  def result[A]: A => Parser[A] = a => Parser(input => List((a, input)))

  def zero: Parser[Unit] = Parser(_ => List())

  def item: Parser[Char] = Parser(inp =>
    inp.toList match {
      case Nil => Nil
      case x :: xs => List((x, xs.mkString))
    })

  def seq[A, B]: Parser[A] => Parser[B] => Parser[(A, B)] = p => q =>
    Parser(input =>
      for {
        a <- p.run(input)
        b <- q.run(a._2)
      } yield ((a._1, b._1), a._2)
    )


}