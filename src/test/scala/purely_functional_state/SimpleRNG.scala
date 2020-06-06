package fpinscala
package chapter6
package tests

import org.scalatest.matchers.should._
import org.scalatest.flatspec.AnyFlatSpec
import fpinscala.chapter6.simple_rng._

class Excercise6_1 extends AnyFlatSpec with Matchers {
  val sRNG = SimpleRNG(42)

  "First non-negative integer of `SimpleRNG` with seed 42" should "be 16159453" in {
    val (i, rng) = RNG.nonNegativeInt(sRNG)

    i should be (16159453)
  }
}

class Excercise6_2 extends AnyFlatSpec with Matchers {
  val sRNG = SimpleRNG(42)

  "First double of `SimpleRNG` with 42 as seed" should "be 16159453" in {
    val (i, rng) = RNG.double(sRNG)

    i should be (0.16159453)
  }
}
