package fpinscala
package chapter4
package tests

import org.scalatest.matchers.should._
import org.scalatest.flatspec.AnyFlatSpec
import opshn._

class Excercise4_1 extends AnyFlatSpec with Matchers {
    def invert(a: Int): Opshn[Double] = a match {
      case 0 => Non
      case _ => Sam(1.0 / a)
    }

    "`map` over an Opshn" should "be Nan if the initial value is Nan" in {
    val n: Opshn[Int] = Non

    n.map(_ + 1) should be (Non)
  }

  it should "be Sam value if the initial value is not Nan" in {
    val s  = Sam(2)

    s.map(_ + 1) should be (Sam(3))
  }

  "`flatMap` over an Opshn" should "be Nan if the initial value is Nan" in {
    val n: Opshn[Int] = Non

    n.flatMap(invert) should be (Non)
  }

  it should "be Nan if the mapping function fails" in {
    val s  = Sam(0)

    s.flatMap(invert) should be (Non)
  }

  it should "be Sam value if the initial value is not Nan and the mapping function succeeds" in {
    val s  = Sam(2)

    s.flatMap(invert) should be (Sam(0.5))
  }

  "`getOrElse`" should "return the default value for Nan" in {
    Non.getOrElse(1) should be (1)
  }

  it should "return the value for Sam(v)" in {
    Sam(2).getOrElse(1) should be (2)
  }
  
  "`orElse`" should "return the default Opshn for Nan" in {
    Non.orElse(Sam(1)) should be (Sam(1))
    Non.orElse(Non) should be (Non)
  }

  it should "return the original Opshn for Sam(v)" in {
    val v  = Sam(2)

    v.orElse(Non) should be (v)
    v.orElse(Sam(1)) should be (v)
  }

  "`filter` over Sam value" should "return Sam value if it satisfies the filtering function" in {
    val v  = Sam(2)
    
    v.filter(_ == 2) should be (v)
  }

  it should "return Nan if does not satisfy the filtering function" in {
    val v  = Sam(2)

    v.filter(_ != 2) should be (Non)
  }

  "`filter` over Nan" should "reurn Nan regardles of the filtering function" in {
    Non.filter(_ => true) should be (Non)
    Non.filter(_ => false) should be (Non)
  }
}