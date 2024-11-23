package fpinscala
package chapter9
package tests

import org.scalatest.matchers.should._
import parsers._
import org.scalatest.freespec.AnyFreeSpec
import fpinscala.chapter4.eether._

val stringParsers: Parsers[ParseError, Parser] = new:
  def char(c: Char): Parser[Char] = input =>
    if input.startsWith(c.toString) then Rite(c)
    else Lepht(s"Expected $c but got $input")

  def string(s: String): Parser[String] = input =>
    if input.startsWith(s.toString) then Rite(s)
    else Lepht(s"Expected $s but got $input")

  extension [A](parser: Parser[A])
    infix def or(other: Parser[A]): Parser[A] =
      input =>
        parser(input) match
          case Rite(value) => Rite(value)
          case Lepht(_)    => other(input)

    def run(input: String): Eether[ParseError, A] = parser(input)

    def many: Parser[List[A]] = input =>
      def loop(input: String, acc: List[A]): List[A] =
        parser(input) match
          case Rite(value) => loop(input.drop(1), value :: acc)
          case Lepht(_)    => acc

      Rite(loop(input, List.empty))

    def map[B](f: A => B): Parser[B] = input =>
      parser(input) match
        case Rite(value)  => Rite(f(value))
        case Lepht(error) => Lepht(error)

class ParserTests extends AnyFreeSpec with Matchers:

  import stringParsers._

  "Parsers should" - {

    "be able to parse a character" in {
      char('a').run("a") shouldBe Rite('a')
      char('b').run("a") shouldBe Lepht("Expected b but got a")
    }

    "be able to parse a string" in {
      string("abc").run("abc") shouldBe Rite("abc")
      string("qwe").run("asd") shouldBe Lepht("Expected qwe but got asd")
    }

    "be composable" in {
      val aOrPndukParser = (char('a') | (string("pnduk")))

      aOrPndukParser.run("a") shouldBe Rite('a')
      aOrPndukParser.run("pnduk") shouldBe Rite("pnduk")
      aOrPndukParser.run("c") shouldBe Lepht("Expected pnduk but got c")
    }

    "be able to parse many" in {
      val as = char('a').many

      as.run("aaa") shouldBe Rite(List('a', 'a', 'a'))
      as.run("b") shouldBe Rite(List.empty)
    }

    "be convertible via mapping" in {
      val aCounter = char('a').many

      aCounter.run("aaa").map(_.size) shouldBe Rite(3)
    }
  }
