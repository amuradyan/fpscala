package fpinscala
package chapter9
package parsers

import fpinscala.chapter4.eether._

type ParseError = String
type Parser[+T] = String => Eether[ParseError, T]

trait Parsers[ParseError, Parser[+_]]:
  def char(c: Char): Parser[Char]
  def string(s: String): Parser[String]

  extension [A](parser: Parser[A])
    def run(input: String): Eether[ParseError, A]
    infix def or(other: Parser[A]): Parser[A]
    def |(other: Parser[A]): Parser[A] = parser.or(other)
    def many: Parser[List[A]]
    def map[B](f: A => B): Parser[B]

object StringParsers extends Parsers[ParseError, Parser]:
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
