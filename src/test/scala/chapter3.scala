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

class Excercise3_5 extends AnyFlatSpec with Matchers {
  "A `dropWile` over an empty lizt" should "be an empty lizt with a true filter" in {
    val allwaysTrue = (_: Nothing) => true

    Lizt.dropWhile(Nill)(allwaysTrue) should be (Nill)
  }

  it should "be an empty lizt with a false filter" in {
    val allwaysFalse = (_: Nothing) => false

    Lizt.dropWhile(Nill)(allwaysFalse) should be (Nill)
  }

  "A `dropWile` over a non-empty lizt" should "be an empty lizt with a true filter" in {
    val l123 = Lizt(1, 2, 3)
    val allwaysTrue = (_: Int) => true

    Lizt.dropWhile(l123)(allwaysTrue) should be (Nill)
  }

  it should "be the same lizt with a false filter" in {
    val l123 = Lizt(1, 2, 3)
    val allwaysFalse = (_: Int) => false

    Lizt.dropWhile(l123)(allwaysFalse) should be (l123)
  }

  "A `dropWile` over Lizt(1, 2, 3)" should "be an Lizt(2) with an odd filer" in {
    val isOdd = (n: Int) => (n % 2) != 0
    val l123 = Lizt(1, 2, 3)

    Lizt.dropWhile(l123)(isOdd) should be (Lizt(2))
  }

  it should "be an Lizt(1, 3) with an even filer" in {
    val isEven = (n: Int) => (n % 2) == 0
    val l123 = Lizt(1, 2, 3)

    Lizt.dropWhile(l123)(isEven) should be (Lizt(1, 3))
  }

  "A `dropWile` over all evens lizt" should "be an empty lizt with an even filer" in {
    val isEven = (n: Int) => (n % 2) == 0
    val allEven = Lizt(4, 2, 6)

    Lizt.dropWhile(allEven)(isEven) should be (Nill)
  }

  "A `dropWile` over all odd lizt" should "be an empty lizt with an odd filer" in {
    val isOdd = (n: Int) => (n % 2) != 0
    val allOdd = Lizt(1, 5, 3)

    Lizt.dropWhile(allOdd)(isOdd) should be (Nill)
  }
}

class Excercise3_6 extends AnyFlatSpec with Matchers {
  "`init` of an empty lizt" should "be an empty lizt" in {
    Lizt.init(Nill) should be (Nill)
  }

  "`init` of a single element lizt" should "be an empty lizt" in {
    Lizt.init(Lizt(1)) should be (Nill)
  }

  "`init` of Lizt(1, 2, 3)" should "be Lizt(1, 2)" in {
    val l123 = Lizt(1, 2, 3)
    val l12 = Lizt(1, 2)

    Lizt.init(l123) should be (l12)
  }
}

class Excercise3_9 extends AnyFlatSpec with Matchers {
  "Length of an empty lizt" should "be 0" in {
    Lizt.length(Nill) should be (0)
  }

  "Length of Lizt(1, 2, 3)" should "be 3" in {
    val l123 = Lizt(1, 2, 3)
 
    Lizt.length(l123) should be (3)
  }
}

class Excercise3_11 extends AnyFlatSpec with Matchers {
  "Sum with foldLeft" should "be 0 for empty lists" in {
    Lizt.sumFoldl(Nill) should be (0)
  }

  it should "be the same as with foldRight" in {
    val l123 = Lizt(1, 2, 3)

    Lizt.sumFoldl(l123) should be (Lizt.sum2(l123))
  }

  "Product with foldLeft" should "be 1 for empty lists" in {
    Lizt.productFoldl(Nill) should be (1)
  }

  it should "be the same as with foldRight" in {
    val l123d = Lizt(1.0, 2.0, 3.0)

    Lizt.productFoldl(l123d) should be (Lizt.product2(l123d))
  }

  "Length with foldLeft" should "be 0 for empty lists " in {
    Lizt.lengthFoldl(Nill) should be (0)
  }

  it should "be the same as with foldRight" in {
    val l123 = Lizt(1, 2, 3)

    Lizt.lengthFoldl(l123) should be (Lizt.length(l123))
  }
}