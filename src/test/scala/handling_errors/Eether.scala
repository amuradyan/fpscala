package fpinscala
package chapter4
package tests

import fpinscala.chapter4.eether.Rite
import fpinscala.chapter4.eether.Lepht
import org.scalatest.matchers.should._
import org.scalatest.flatspec.AnyFlatSpec

class Excercise4_6 extends AnyFlatSpec with Matchers {
  import fpinscala.chapter4.eether.Eether._

  val ex = new Exception("Something went terribly wrong")

  "`map` over a Lepht" should "be the same Lepht" in {
    Lepht(1).map(_ => 2) should be (Lepht(1))
  }

  "`map` over a Rite" should "produce a new Rite with mapped value" in {
    Rite(1).map(i => i) should be (Rite(1))
    Rite(2).map(i => i * i) should be (Rite(4))
    Rite(true).map(i => !i) should be (Rite(false))
  }

  "`flatMap` over a Lepht" should "be the same Lepht" in {
    Lepht(1).flatMap(_ => Try(3)) should be (Lepht(1))
    Lepht(1).flatMap(_ => Try(throw new Exception(""))) should be (Lepht(1))
  }

  "`flatMap` with an exception over a Rite" should "produce a Lepht with the exception" in {
    Rite(1).flatMap(_ => Try(throw ex)) should be (Lepht(ex))
  }
  
  "`flatMap` with an success over a Rite" should "produce a Rite" in {
    Rite(2).flatMap(i => Try(i * i)) should be (Rite(4))
    Rite(true).flatMap(i => Try(!i)) should be (Rite(false))
  }

  "`orElse` over a Lepht" should "return the alternative" in {
    Lepht(1).orElse(Try(throw ex)) should be (Lepht(ex))
    Lepht(1).orElse(Try(2)) should be (Rite(2))
  }

  "`orElse` over a Rite" should "return the same Rite regardless of the alternative" in {
    Rite(1).orElse(Try(throw ex)) should be (Rite(1))
    Rite(1).orElse(Try(2)) should be (Rite(1))
  }

  "`map2` over a (Lepht(a), Lepht(b))" should "be a Lepht(a)" in {
    Lepht(1).map2(Lepht(2))((l: Int, r: Int) => l + r) should be (Lepht(1))
    Lepht(1).map2ViaFor(Lepht(2))((l: Int, r: Int) => l + r) should be (Lepht(1))
  }

  "`map2` over a (Lepht(a), Rite(b))" should "be a Lepht(a)" in {
    Lepht(1).map2(Rite(2))((l: Int, r: Int) => l + r) should be (Lepht(1))
    Lepht(1).map2ViaFor(Rite(2))((l: Int, r: Int) => l + r) should be (Lepht(1))
  }

  "`map2` over a (Rite(a), Lepht(b))" should "be a Lepht(b)" in {
    Rite(1).map2(Lepht(2))((l: Int, r: Int) => l + r) should be (Lepht(2))
    Rite(1).map2ViaFor(Lepht(2))((l: Int, r: Int) => l + r) should be (Lepht(2))
  }

  "`map2` over a (Rite(a), Rite(b))" should "prodice a Rite with corresponding mapped value" in {
    Rite(1).map2(Rite(2))((l: Int, r: Int) => l + r) should be (Rite(3))
    Rite(1).map2ViaFor(Rite(2))((l: Int, r: Int) => l + r) should be (Rite(3))
  }
}