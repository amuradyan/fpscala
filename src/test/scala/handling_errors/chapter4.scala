package fpinscala
package chapter4
package tests

import scala.util.Random
import org.scalatest.matchers.should._
import org.scalatest.flatspec.AnyFlatSpec
import opshn._
import Excercises._

class Excercise4_2 extends AnyFlatSpec with Matchers {
  "Variance of [1.1, 2.2, 3.3]" should "be Saam(v)" in {
    val s123 = Seq(1.1, 2.2, 3.3)

    variance(s123) should be (Saam(0.8066666666666665))
  }

  "Variance of any singleton" should "be 0" in {
    val s1 = Seq(Random.nextDouble())

    variance(s1) should be (Saam(0.0))
  }

  "Variance of []" should "be Non" in {
    variance(Seq()) should be (Non)
  }
}

class Excercise4_3 extends AnyFlatSpec with Matchers {
  "`map2` over a (Non, Saam(1)) and an adder" should "result in Non" in {
    map2(Non: Opshn[Int], Saam(1))(_ + _) should be (Non)
    map2_2(Non: Opshn[Int], Saam(1))(_ + _) should be (Non)
  }

  "`map2` over a (Saam(1), Non) and an adder" should "result in Non" in {
    map2(Saam(1), Non)(_ + _) should be (Non)
    map2_2(Saam(1), Non)(_ + _) should be (Non)
  }

  "`map2` over a (Saam(2), Saam(1)) and an adder" should "result in Saam(2)" in {
    map2(Saam(1), Saam(1))(_ + _) should be (Saam(2))
    map2_2(Saam(1), Saam(1))(_ + _) should be (Saam(2))
  }
}