package fpinscala
package chapter6
package simple_rng

import fpscala.chapter6.simple_rng.SimpleRNG
import org.scalatest.matchers.should._
import org.scalatest.flatspec.AnyFlatSpec

class Excercise6_1 extends AnyFlatSpec with Matchers {
  val sRNG = SimpleRNG(42)

  "First number of `SimpleRNG` with 42 as seed" should "be 16159453" in {
    val (i, rng) = sRNG.nextInt

    i should be (16159453)
  }
}
