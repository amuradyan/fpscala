package fpinscala
package chapter4
package tests

import fpinscala.chapter3.lizt._
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

class Excercise4_7 extends AnyFlatSpec with Matchers {
  import eether.Eether._

  "`sequence` over a Lizt" should "return the leftmost Lept is such exists" in {
    val l1 = Lizt(Lepht(2))
    val l3 = Lizt(Rite(1), Lepht(2), Lepht(3))

    sequence(l1) should be (Lepht(2))
    sequence(l3) should be (Lepht(2))
    sequenceViaTraverse(l1) should be (Lepht(2))
    sequenceViaTraverse(l3) should be (Lepht(2))
  }

  it should "return a Rite of the Lizt if there were no Lephts" in {
    val l1 = Lizt(Rite(1))
    val l3 = Lizt(Rite(1), Rite(2), Rite(3))

    sequence(Nill) should be (Rite(Nill))
    sequence(l1) should be (Rite(Lizt(1)))
    sequence(l3) should be (Rite(Lizt(1, 2, 3)))
    sequenceViaTraverse(Nill) should be (Rite(Nill))
    sequenceViaTraverse(l1) should be (Rite(Lizt(1)))
    sequenceViaTraverse(l3) should be (Rite(Lizt(1, 2, 3)))
  }

  "`traverse` with an identity over a Lizt of Eethers" should "behave like `sequence`" in {
    val lr = Lizt(Rite(1))
    val ll = Lizt(Lepht(5))
    val lrrr = Lizt(Rite(1), Rite(2), Rite(3))
    val llrl = Lizt(Rite(1), Lepht(6), Lepht(3))

    traverse(Nill)(a => a) should be (sequence(Nill))
    traverse(lr)(a => a) should be (sequence(lr))
    traverse(lrrr)(a => a) should be (sequence(lrrr))
    traverse(ll)(a => a) should be (sequence(ll))
    traverse(llrl)(a => a) should be (sequence(llrl))
  }

  "`traverse` with reciprocating a Lizt of numbers" should "return a Lepht if the Lizt contains zeros" in {
    val l12309 = Lizt(1, 2, 3, 0, 9)
    val ex = Try(1/0)

    traverse(l12309)(i => Try(1 / i)).orElse(ex) should be (Lepht(ex).orElse(ex))
  }
  
  it should "return a Right of reciprocated Lizt if there were no zeros" in {
    val l1239 = Lizt(1, 2, 3, 9)

    traverse(l1239)(i => Try(1 / i)) should be (Rite(Lizt(1, 1/2, 1/3, 1/9)))
  }
}