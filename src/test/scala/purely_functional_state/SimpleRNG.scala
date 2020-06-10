package fpinscala
package chapter6
package tests

import fpinscala.chapter3.lizt.Nill
import fpinscala.chapter3.lizt.Lizt
import org.scalatest.matchers.should._
import org.scalatest.flatspec.AnyFlatSpec
import fpinscala.chapter6.simple_rng._
import fpinscala.chapter6.simple_rng.RNG.Rand

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
  
  "First `doubleInt` of `SimpleRNG` with 42 as seed" should "be (0.16159453,-1281479697)" in {
    RNG.doubleInt(sRNG) should be (0.16159453,-1281479697)
  }

  "First `double3` of `SimpleRNG` with 42 as seed" should "be (0.16159453, 0.1281479696, 0.340305901)" in {
    RNG.double3(sRNG) should be (0.16159453, 0.1281479696, 0.340305901)
  }
}

class Excercise6_4 extends AnyFlatSpec with Matchers {
  val sRNG = SimpleRNG(42)

  "Asking for 0 `ints` from `SimpleRNG` with 42 as seed" should "return the Nill and the original RNG" in {
    RNG.ints(0)(sRNG) should be ((Nill, sRNG))
  }

  "Asking for 2 `ints` from `SimpleRNG` with 42 as seed" should "return the Lizt(-1281479697, 16159453) and the second RNG" in {
    val r2 = sRNG
      .nextInt._2
      .nextInt._2

    RNG.ints(2)(sRNG) should be ((Lizt(-1281479697, 16159453), r2))
  }

  "Asking for 3 `ints` from `SimpleRNG` with 42 as seed" should "return the Lizt(-340305902, -1281479697, 16159453) and the third RNG" in {
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

class Excercise6_6 extends AnyFlatSpec with Matchers {
  val sRNG = SimpleRNG(42)

  "`map2`-ing over two ints with string concatenation via`SimpleRNG` with 42 as seed" should "return 161594531281479696" in {
    RNG.map2(RNG.nonNegativeInt, RNG.nonNegativeInt)((a, b) => s"$a$b")(sRNG)._1 should be ("161594531281479696")
  }
}

class Excercise6_7 extends AnyFlatSpec with Matchers {
  val sRNG = SimpleRNG(42)

  "`sequence`-ing over three random int generators" should "yield the list of the three random numbers" in {
    val (_, r3) = RNG.ints(3)(sRNG)

    RNG.sequence(Lizt(RNG.int, RNG.int, RNG.int))(sRNG) should be ((Lizt(16159453, -1281479697, -340305902), r3))
  }

  "`sequence`-ing over an empty Lizt" should "yield an empty Lizt" in {
    val (_, r3) = RNG.ints(3)(sRNG)

    RNG.sequence(Nill)(sRNG) should be (Nill, sRNG)
  }
}

class Excercise6_8 extends AnyFlatSpec with Matchers {
  val sRNG = SimpleRNG(42)

  def stringify[A](a: A): Rand[String] = ((a.toString), _)

  "`flatMap`-ing over an Int with stringification" should "yield the String value of the Int" in {
    val (_, r) = sRNG.nextInt

    RNG.flatMap(RNG.int)(stringify)(sRNG) should be (("16159453", r))
  }
}

class Misc extends AnyFlatSpec with Matchers {
  val sRNG = SimpleRNG(42)

  "`both` over two ints with via`SimpleRNG` with 42 as seed" should "return (16159453, 1281479696)" in {
    RNG.both(RNG.nonNegativeInt, RNG.nonNegativeInt)(sRNG)._1 should be ((16159453, 1281479696))
  }

  "`intDoubleViaMap`" should "behave as `intDouble`" in {
    RNG.intDoubleViaMap2(sRNG)._1 should be (RNG.intDouble(sRNG))
  }

  "`doubleIntViaMap`" should "behave as `doubleInt`" in {
    RNG.doubleIntViaMap2(sRNG)._1 should be (RNG.doubleInt(sRNG))
  }
}