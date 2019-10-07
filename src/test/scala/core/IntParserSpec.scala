package core

import core.IntParser._
import org.scalatest._

class IntParserSpec extends FlatSpec with Matchers {
  "naturalNumber parser" should "parse natural number  " in {
    assertResult(naturalNumber.run("-123"))(List())
    assertResult(naturalNumber.run("123"))(List((123, ""), (12, "3"), (1, "23")))
  }

  "int parser" should " parse negative and positive number " in {
    assertResult(int.run("-123"))(List((-123, ""), (-12, "3"), (-1, "23")))
    assertResult(int.run("123"))(List((123, ""), (12, "3"), (1, "23")))
  }

  "ints parser" should "parse a non-empty list of integer" in {
    assertResult(ints.run("[1,4,-32,4]"))(List((List(1, 4, -32, 4), "")))
  }


  "A expr" should "parsing arithmetic expression " in {
    assertResult(expr.run("1+2-(3+4)"))(List())
  }

}
