package fpinscala
package chapter3
package tests

import org.scalatest.matchers.should._
import org.scalatest.flatspec.AnyFlatSpec
import Chapter3._

class Excercise3_2 extends AnyFlatSpec with Matchers {
  "`tail` of a lizt" should "be Nil for an empty lizt" in {
    val emptyLizt = Nill

    Lizt.tail(emptyLizt) should be (Nill)
  }

  it should "be Lizt(2, 3) for Lizt(1, 2, 3)" in {
    val l123 = Lizt(1, 2, 3)

    Lizt.tail(l123) should be (Lizt(2, 3))
  }

  it should "be Lizt('b', 'c') for Lizt('a', 'b', 'c')" in {
    val labc = Lizt('a', 'b', 'c')

    Lizt.tail(labc) should be (Lizt('b', 'c'))
  }
}

class Excercise3_3 extends AnyFlatSpec with Matchers {
  "Setting 1 as head to a lizt" should "return Lizt(1) if initial lizt is empty" in {
    Lizt.setHead(1, Nill) should be (Lizt(1))
  }

  it should "return Lizt(1) for a single element lizt" in {
    Lizt.setHead(1, Lizt(3)) should be (Lizt(1))
  }

  it should "return Lizt(1, 2) for a Lizt(3, 2)" in {
    Lizt.setHead(1, Lizt(3, 2)) should be (Lizt(1, 2))
  }
}

class Excercise3_4 extends AnyFlatSpec with Matchers {
  "Dropping more elements than the lizt contains" should "return Nill" in {
    Lizt.drop(Lizt(1), 3) should be (Nill)
  }

  "Dropping the first element of any lizt" should "be equivalent to its tail" in {
    val l123 = Lizt(1, 2, 3)

    Lizt.drop(l123, 1) should be (Lizt.tail(l123))
  }

  "Dropping the first element of Lizt('a', 'b', 'c')" should "be Lizt('b', 'c')" in {
    val labc = Lizt('a', 'b', 'c')

    Lizt.drop(labc, 1) should be (Lizt('b', 'c'))
  }

  "Dropping a negative number of elements from a lizt" should "be the same lizt" in {
    val labc = Lizt('a', 'b', 'c')

    Lizt.drop(labc, -1) should be (labc)
  }

  "Dropping all elements of a lizt" should "return an empty lizt" in {
    Lizt.drop(Lizt(1, 2), 2) should be (Nill)
  }

  "Dropping the first two elements of Lizt('a', 'b', 'c')" should "be Lizt('c')" in {
    val labc = Lizt('a', 'b', 'c')

    Lizt.drop(labc, 2) should be (Lizt('c'))
  }
}