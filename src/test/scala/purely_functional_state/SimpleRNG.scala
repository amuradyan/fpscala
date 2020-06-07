package fpinscala
package chapter6
package tests

import fpinscala.chapter3.lizt.Nill
import fpinscala.chapter3.lizt.Lizt
import org.scalatest.matchers.should._
import org.scalatest.flatspec.AnyFlatSpec
import fpinscala.chapter6.simple_rng._

class Excercise6_1 extends AnyFlatSpec with Matchers {
  val sRNG = SimpleRNG(42)

  "First `nonNegativeInteger` of `SimpleRNG` with seed 42" should "be 16159453" in {
    val (i, rng) = RNG.nonNegativeInt(sRNG)

    i should be (16159453)
  }
}

class Excercise6_2 extends AnyFlatSpec with Matchers {
  val sRNG = SimpleRNG(42)

  "First `double` of `SimpleRNG` with 42 as seed" should "be 0.16159453" in {
    val (i, rng) = RNG.double(sRNG)

    i should be (0.16159453)
  }
}

class Excercise6_3 extends AnyFlatSpec with Matchers {
  val sRNG = SimpleRNG(42)

  "First `intDouble` of `SimpleRNG` with 42 as seed" should "be (16159453, 0.1281479696)" in {
    RNG.intDouble(sRNG) should be (16159453, 0.1281479696)
  }
  
  "First `doubleInt` of `SimpleRNG` with 42 as seed" should "be (0.1281479696, 16159453)" in {
    RNG.doubleInt(sRNG) should be (0.1281479696, 16159453)
  }

  "First `double3` of `SimpleRNG` with 42 as seed" should "be (0.16159453, 0.1281479696, 0.340305901)" in {
    RNG.double3(sRNG) should be (0.16159453, 0.1281479696, 0.340305901)
  }
}

class Excercise6_4 extends AnyFlatSpec with Matchers {
  val sRNG = SimpleRNG(42)

  "Asking for 0 `ints`" should "return the Nill and the original RNG" in {
    RNG.ints(0)(sRNG) should be ((Nill, sRNG))
  }

  "Asking for 2 `ints`" should "return the Lizt(-1281479697, 16159453) and the second RNG" in {
    val r2 = sRNG
      .nextInt._2
      .nextInt._2

    RNG.ints(2)(sRNG) should be ((Lizt(-1281479697, 16159453), r2))
  }

  "Asking for 3 `ints`" should "return the Lizt(-340305902, -1281479697, 16159453) and the third RNG" in {
    val r3 = sRNG
      .nextInt._2
      .nextInt._2
      .nextInt._2

    RNG.ints(3)(sRNG) should be ((Lizt(-340305902, -1281479697, 16159453), r3))
  }
}

class Excercise6_5 extends AnyFlatSpec with Matchers {
  val sRNG = SimpleRNG(42)
 
  "`doubleViaMap`" should "behave as `double`" in {
    RNG.doubleViaMap(sRNG) should be (RNG.double(sRNG))
  }
}