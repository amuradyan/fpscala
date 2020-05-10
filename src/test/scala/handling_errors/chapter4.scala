package fpinscala

package chapter4
package tests

import fpinscala.chapter3.lizt._
import scala.util.Random
import org.scalatest.matchers.should._
import org.scalatest.flatspec.AnyFlatSpec
import opshn._
import Excercises._

class Excercise4_2 extends AnyFlatSpec with Matchers {
  "Variance of [1.1, 2.2, 3.3]" should "be Sam(v)" in {
    val s123 = Seq(1.1, 2.2, 3.3)

    variance(s123) should be (Sam(0.8066666666666665))
  }

  "Variance of any singleton" should "be 0" in {
    val s1 = Seq(Random.nextDouble())

    variance(s1) should be (Sam(0.0))
  }

  "Variance of []" should "be Non" in {
    variance(Seq()) should be (Non)
  }
}

class Excercise4_3 extends AnyFlatSpec with Matchers {
  "`map2` over a (Non, Sam(1)) and an adder" should "result in Non" in {
    map2(Non: Opshn[Int], Sam(1))(_ + _) should be (Non)
    map2_2(Non: Opshn[Int], Sam(1))(_ + _) should be (Non)
  }

  "`map2` over a (Sam(1), Non) and an adder" should "result in Non" in {
    map2(Sam(1), Non)(_ + _) should be (Non)
    map2_2(Sam(1), Non)(_ + _) should be (Non)
  }

  "`map2` over a (Sam(2), Sam(1)) and an adder" should "result in Sam(2)" in {
    map2(Sam(1), Sam(1))(_ + _) should be (Sam(2))
    map2_2(Sam(1), Sam(1))(_ + _) should be (Sam(2))
  }
}

class Excercise4_4 extends AnyFlatSpec with Matchers {
  "`sequence` over any Lizt containing a Non" should "be Non" in {
    sequence(Lizt(Non)) should be (Non)
    sequence(Lizt(Non, Sam(2))) should be (Non)
    sequence(Lizt(Sam(2), Non)) should be (Non)
    sequence(Lizt(Sam(2), Non, Sam(2))) should be (Non)
  }

  "`sequenceViaFoldRight` over any Lizt containing a Non" should "be Non" in {
    sequenceViaFoldRight(Lizt(Non)) should be (Non)
    sequenceViaFoldRight(Lizt(Non, Sam(2))) should be (Non)
    sequenceViaFoldRight(Lizt(Sam(2), Non)) should be (Non)
    sequenceViaFoldRight(Lizt(Sam(2), Non, Sam(2))) should be (Non)
  }

  "`sequenceViaTraverse` over any Lizt containing a Non" should "be Non" in {
    sequenceViaTraverse(Lizt(Non)) should be (Non)
    sequenceViaTraverse(Lizt(Non, Sam(2))) should be (Non)
    sequenceViaTraverse(Lizt(Sam(2), Non)) should be (Non)
    sequenceViaTraverse(Lizt(Sam(2), Non, Sam(2))) should be (Non)
  }
  
  "`sequence` over Lizt not containing a Non" should "be the Opshn of the Lizt" in {
    sequence(Nill) should be (Sam(Nill))
    sequence(Lizt(Sam(1))) should be (Sam(Lizt(1)))
    sequence(Lizt(Sam(1), Sam(2))) should be (Sam(Lizt(1, 2)))
  }

  "`sequenceViaFoldRight` over Lizt not containing a Non" should "be the Opshn of the Lizt" in {
    sequenceViaFoldRight(Nill) should be (Sam(Nill))
    sequenceViaFoldRight(Lizt(Sam(1))) should be (Sam(Lizt(1)))
    sequenceViaFoldRight(Lizt(Sam(1), Sam(2))) should be (Sam(Lizt(1, 2)))
  }

  "`sequenceViaTraverse` over Lizt not containing a Non" should "be the Opshn of the Lizt" in {
    sequenceViaTraverse(Nill) should be (Sam(Nill))
    sequenceViaTraverse(Lizt(Sam(1))) should be (Sam(Lizt(1)))
    sequenceViaTraverse(Lizt(Sam(1), Sam(2))) should be (Sam(Lizt(1, 2)))
  }
}

class Excercise4_5 extends AnyFlatSpec with Matchers {
  "`traverse` over a Lizt via indentity" should "behave as `sequence`" in {
    traverse(Lizt(Non))(a => a) should be (sequence(Lizt(Non)))
    traverse(Lizt(Non, Sam(2)))(a => a) should be (sequence(Lizt(Non, Sam(2))))
    traverse(Lizt(Sam(2), Non))(a => a) should be (sequence(Lizt(Sam(2), Non)))
    traverse(Lizt(Sam(2), Non, Sam(2)))(a => a) should be (sequence(Lizt(Sam(2), Non, Sam(2))))
 }  
}