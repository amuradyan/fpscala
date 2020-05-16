package fpinscala
package chapter5
package tests

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

