package core

import org.scalatest._

class ParserSpec extends FlatSpec with Matchers {

  import Parser._

  "A Parser" should "primitive parser result which succeed without consuming any of the input String" in {
    assertResult(result("a").run(""))(List(("a", "")))
  }

  it should "primitive parser zero always failed " in {
    assertResult(zero.run(""))(List())
  }

  it should "item primitive parser whcih return first char from input " in {
    assertResult(item.run("hello"))(List(('h',"ello")))
    assertResult(item.run(""))(List())
  }

  it should "seq combinator sequencing parser " in {
    assertResult(seq(item)(item).run("hello"))(List((('h','e'),"ello")))
  }

}
