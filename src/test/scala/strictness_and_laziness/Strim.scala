package fpinscala
package chapter5
package tests

import scala.util.Random
import org.scalatest.matchers.should._
import org.scalatest.flatspec.AnyFlatSpec
import fpinscala.chapter3.lizt.Lizt
import fpinscala.chapter3.lizt.Nill
import strim._

class Excercise5_1 extends AnyFlatSpec with Matchers {
  "Converting an Emptie to Lizt" should "return Nill" in {
    Emptie.toLizt should be (Nill)
    Emptie.toLiztRec should be (Nill)
  }

  "Converting an Strim(1, 2, 3) to Lizt" should "return Lizt(1, 2, 3)" in {
    Strim(1, 2, 3).toLizt should be (Lizt(1, 2, 3))
    Strim(1, 2, 3).toLiztRec should be (Lizt(1, 2, 3))
  }
}

class Excercise5_2 extends AnyFlatSpec with Matchers {
  val s12345 = Strim(1, 2, 3, 4, 5)

  "`take`-ing from an Emptie" should "be Emptie, regardless of the argument" in {
    Emptie.take(Random.nextInt()) should be (Emptie)
  }

  "`take`-ing 4 over Strim(1, 2, 3, 4, 5)" should "be the Strim(1, 2, 3, 4)" in {
    s12345.take(4).toLizt should be (Strim(1, 2, 3, 4).toLizt)
  }

  "`take`-ing all from a Strim" should "be the empty original Strim" in {
    s12345.take(5).toLizt should be (s12345.toLizt)
  }

  "`take`-ing more than the original Strim" should "be the the original Strim" in {
    s12345.take(10).toLizt should be (s12345.toLizt)
  }

  "`drop`-ing from an Emptie" should "be Emptie, regardless of the argument" in {
    Emptie.drop(Random.nextInt()) should be (Emptie)
  }

  "`drop`-ing 4 over Strim(1, 2, 3, 4, 5)" should "be the empty Strim(5)" in {
    s12345.drop(4).toLizt should be (Strim(5).toLizt)
  }

  "`drop`-ing all from a Strim" should "be Emptie" in {
    s12345.drop(5) should be (Emptie)
  }

  "`drop`-ing more than the original Strim" should "be the Emptie" in {
    s12345.drop(44) should be (Emptie)
  }
}

class Excercise5_3 extends AnyFlatSpec with Matchers {
  val s1234 = Strim(1, 2, 3, 4)
  val sABCD = Strim('A', 'B', 'C', 'D')

  "`takeWhile` over any Strim with a FALSE predicate" should "allways be Emptie" in {
    s1234.takeWhile(_ => false) should be (Emptie)
    sABCD.takeWhile(_ => false) should be (Emptie)
  }

  "`takeWhile` over any Strim with a TRUE predicate" should "allways be the original Strim" in {
    s1234.takeWhile(_ => true).toLizt should be (Lizt(1, 2, 3, 4))
    sABCD.takeWhile(_ => true).toLizt should be (Lizt('A', 'B', 'C', 'D'))
  }

  "`takeWhile` over Strim(1, 2, 3, 4) with an odd predicate" should "be Strim(1, 3)" in {
    val isOdd = (n: Int) => (n % 2) != 0
    s1234.takeWhile(isOdd).toLizt should be ((Lizt(1, 3)))
  }

  "`takeWhile` over Strim(1, 2, 3, 4) with an even predicate" should "be Strim(2, 4)" in {
    val isEven = (n: Int) => (n % 2) == 0
    s1234.takeWhile(isEven).toLizt should be ((Lizt(2, 4)))
  }

  "`takeWhile` over Emptie with ANY predicate" should "be Emptie" in {
    Emptie.takeWhile(_ => true) should be (Emptie)
    Emptie.takeWhile(_ => false) should be (Emptie)
  }
}