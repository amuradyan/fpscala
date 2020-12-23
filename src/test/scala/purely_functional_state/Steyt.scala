package fpinscala
package chapter6
package tests

import fpinscala.chapter3.lizt.Lizt
import fpinscala.chapter3.lizt.Nill
import fpinscala.chapter6.steyt.Steyt
import fpinscala.chapter6.simple_rng.SimpleRNG
import fpinscala.chapter6.simple_rng.RNG
import fpinscala.chapter6.simple_rng.RNG.Rand
import org.scalatest.matchers.should.Matchers
import org.scalatest.flatspec.AnyFlatSpec

class Exercise6_10 extends AnyFlatSpec with Matchers {
  val sRNG = SimpleRNG(42)

  def prependZero(n: Int): Double = s"0.${n.toString()}".toDouble
  def stringify[A](a: A): Rand[String] = ((a.toString), _)
  def stringifySteyt[A](a: A): Steyt[RNG, String] = Steyt(((a.toString), _))
  def strConcat[A, B](a: A, b: B): String = s"$a$b"

  "`map` on Steyt with RNG" should "behave as `map` on RNG" in {
    Steyt(RNG.nonNegativeInt).map(prependZero).run(sRNG) should be (RNG.doubleViaMap(sRNG))
  }

  "`map2` on Steyt with RNG" should "behave as `map2` on RNG" in {
    Steyt(RNG.nonNegativeInt).map2(Steyt(RNG.nonNegativeInt))(strConcat).run(sRNG) should be (RNG.map2(RNG.nonNegativeInt, RNG.nonNegativeInt)(strConcat)(sRNG))
  }

  "`flatMap` on Steyt with RNG" should "behave as `flatMap` on RNG" in {
    Steyt(RNG.int).flatMap(stringifySteyt).run(sRNG) should be (RNG.flatMap(RNG.int)(stringify)(sRNG))
  }

  "`sequence` on Steyt over three random int generators" should "behave as `sequence` on RNG" in {
    Steyt.sequence(Lizt(Steyt(RNG.int), Steyt(RNG.int), Steyt(RNG.int))).run(sRNG) should be (RNG.sequence(Lizt(RNG.int, RNG.int, RNG.int))(sRNG))
  }

  "`sequence` on Steyt over an empty Lizt" should "behave as `sequence` on RNG" in {
    Steyt.sequence(Nill).run(sRNG) should be (RNG.sequence(Nill)(sRNG))
  }
}