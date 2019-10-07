package core

import org.scalatest._

class ParserSpec extends FlatSpec with Matchers {

  import IntParser._
  import Parser._

  "A Parser" should "primitive parser result which succeed without consuming any of the input String" in {
    assertResult(result("a").run(""))(List(("a", "")))
  }

  it should " failed with zero parser" in {
    assertResult(zero.run(""))(List())
  }

  it should "which return first char from input using item primitive parser " in {
    assertResult(item.run("hello"))(List(('h', "ello")))
    assertResult(item.run(""))(List())
  }

  it should "seq  parser " in {
    assertResult(seq(item)(item).run("hello"))(List((('h', 'e'), "llo")))
  }
  it should "bind  monadic sequencing combinator for sequencing parsers" in {
    val actual = bind(item)(a => result(a)).run("hello")
    assertResult(actual)(List(('h', "ello")))
  }
  it should "sat that takes predicates it return result if satisfies and fails otherwise " in {
    val actual1 = sat(a => a == 'h').run("hello")
    assertResult(actual1)(List(('h', "ello")))

    val actual2 = sat(a => a == 'h').run("ello")
    assertResult(actual2)(List())
  }

  it can "define parsers for recognizing characters" in {
    val actual = char('h').run("hllo")
    assertResult(actual)(List(('h', "llo")))
  }

  it can "define parsers for specific digit" in {
    val actual = char('9').run("904323")
    assertResult(actual)(List(('9', "04323")))
  }

  it can "define parsers for specific lower" in {
    val actual = lower.run("helo")
    assertResult(actual)(List(('h', "elo")))
  }

  it can "define parsers for specific upper" in {
    val actual = upper.run("Helo")
    assertResult(actual)(List(('H', "elo")))
    assertResult(upper.run("helo"))(List())
  }

  it should "choice between parsers with plus combinator" in {
    val lowerUpper = lower.plus(upper)
    assertResult(lowerUpper.run("Bcd"))(List(('B', "cd")))
    assertResult(lowerUpper.run("cd"))(List(('c', "d")))
  }

  it should "letter combinator" in {
    assertResult(letter.run("Bcd"))(List(('B', "cd")))
    assertResult(letter.run("cd"))(List(('c', "d")))
  }
  it should "alphaNum combinator" in {
    assertResult(alphaNum.run("Bcd"))(List(('B', "cd")))
    assertResult(alphaNum.run("1d"))(List(('1', "d")))
  }

  it should "word parser" in {
    assertResult(word.run("Yes!"))(List(("Yes", "!"),
      ("Ye", "s!"), ("Y", "es!"), ("", "Yes!")))

    assertResult(word.run("121"))(List(("", "121")))
  }

  it should "recognize string using string parser" in {
    assertResult(string("hello").run("hello there"))(List(("hello", " there")))
    // assertResult(string("hello").run("helicopter"))(List())
  }

  it should "The many combinator parse sequences of item " in {
    assertResult(many(char('a')).run("aab"))(List((List('a', 'a'), "b"), (List('a'), "ab"), (List(), "aab")))
  }

  it should "The many1 combinator parse sequences of items and return non-empty sequence of item" in {
    assertResult(many1(char('a')).run("aab"))(List((List('a', 'a'), "b"), (List('a'), "ab")))
  }

  it should " sepby for possibly-empty sequences" in {
    assertResult(sepby(int)(char('-')).run("1-2"))(List((List(1, 2), ""), (List(1), "-2"), (List(), "1-2")))
  }
  "bracket parser" should "parser elements inside the brackets" in {
    assertResult(bracket(char('('))(sepby1(int)(char(',')))(char(')')).run("(1,2,3,5)"))(List((List(1, 2, 3, 5), "")))
  }

}
