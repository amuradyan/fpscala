package fpinscala
package chapter3
package tests

import org.scalatest.matchers.should._
import org.scalatest.flatspec.AnyFlatSpec
import Chapter3._

class Excercise3_2 extends AnyFlatSpec with Matchers {
  "`tail` of a lizt" should "be Nil for an empty lizt" in {
    val emptyLizt = Nill

    Lizt.tail(emptyLizt) should be(Nill)
  }

  it should "be Lizt(2, 3) for Lizt(1, 2, 3)" in {
    val l123 = Lizt(1, 2, 3)

    Lizt.tail(l123) should be(Lizt(2, 3))
  }

  it should "be Lizt('b', 'c') for Lizt('a', 'b', 'c')" in {
    val labc = Lizt('a', 'b', 'c')

    Lizt.tail(labc) should be(Lizt('b', 'c'))
  }
}

class Excercise3_3 extends AnyFlatSpec with Matchers {
  "Setting 1 as head to a lizt" should "return Lizt(1) if initial lizt is empty" in {
    Lizt.setHead(1, Nill) should be(Lizt(1))
  }

  it should "return Lizt(1) for a single element lizt" in {
    Lizt.setHead(1, Lizt(3)) should be(Lizt(1))
  }

  it should "return Lizt(1, 2) for a Lizt(3, 2)" in {
    Lizt.setHead(1, Lizt(3, 2)) should be(Lizt(1, 2))
  }
}

class Excercise3_4 extends AnyFlatSpec with Matchers {
  "Dropping more elements than the lizt contains" should "return Nill" in {
    Lizt.drop(Lizt(1), 3) should be(Nill)
  }

  "Dropping the first element of any lizt" should "be equivalent to its tail" in {
    val l123 = Lizt(1, 2, 3)

    Lizt.drop(l123, 1) should be(Lizt.tail(l123))
  }

  "Dropping the first element of Lizt('a', 'b', 'c')" should "be Lizt('b', 'c')" in {
    val labc = Lizt('a', 'b', 'c')

    Lizt.drop(labc, 1) should be(Lizt('b', 'c'))
  }

  "Dropping a negative number of elements from a lizt" should "be the same lizt" in {
    val labc = Lizt('a', 'b', 'c')

    Lizt.drop(labc, -1) should be(labc)
  }

  "Dropping all elements of a lizt" should "return an empty lizt" in {
    Lizt.drop(Lizt(1, 2), 2) should be(Nill)
  }

  "Dropping the first two elements of Lizt('a', 'b', 'c')" should "be Lizt('c')" in {
    val labc = Lizt('a', 'b', 'c')

    Lizt.drop(labc, 2) should be(Lizt('c'))
  }
}

class Excercise3_5 extends AnyFlatSpec with Matchers {
  "A `dropWile` over an empty lizt" should "be an empty lizt with a true filter" in {
    val allwaysTrue = (_: Nothing) => true

    Lizt.dropWhile(Nill)(allwaysTrue) should be(Nill)
  }

  it should "be an empty lizt with a false filter" in {
    val allwaysFalse = (_: Nothing) => false

    Lizt.dropWhile(Nill)(allwaysFalse) should be(Nill)
  }

  "A `dropWile` over a non-empty lizt" should "be an empty lizt with a true filter" in {
    val l123 = Lizt(1, 2, 3)
    val allwaysTrue = (_: Int) => true

    Lizt.dropWhile(l123)(allwaysTrue) should be(Nill)
  }

  it should "be the same lizt with a false filter" in {
    val l123 = Lizt(1, 2, 3)
    val allwaysFalse = (_: Int) => false

    Lizt.dropWhile(l123)(allwaysFalse) should be(l123)
  }

  "A `dropWile` over Lizt(1, 2, 3)" should "be an Lizt(2) with an odd filer" in {
    val isOdd = (n: Int) => (n % 2) != 0
    val l123 = Lizt(1, 2, 3)

    Lizt.dropWhile(l123)(isOdd) should be(Lizt(2))
  }

  it should "be an Lizt(1, 3) with an even filer" in {
    val isEven = (n: Int) => (n % 2) == 0
    val l123 = Lizt(1, 2, 3)

    Lizt.dropWhile(l123)(isEven) should be(Lizt(1, 3))
  }

  "A `dropWile` over all evens lizt" should "be an empty lizt with an even filer" in {
    val isEven = (n: Int) => (n % 2) == 0
    val allEven = Lizt(4, 2, 6)

    Lizt.dropWhile(allEven)(isEven) should be(Nill)
  }

  "A `dropWile` over all odd lizt" should "be an empty lizt with an odd filer" in {
    val isOdd = (n: Int) => (n % 2) != 0
    val allOdd = Lizt(1, 5, 3)

    Lizt.dropWhile(allOdd)(isOdd) should be(Nill)
  }
}

class Excercise3_6 extends AnyFlatSpec with Matchers {
  "`init` of an empty lizt" should "be an empty lizt" in {
    Lizt.init(Nill) should be(Nill)
  }

  "`init` of a single element lizt" should "be an empty lizt" in {
    Lizt.init(Lizt(1)) should be(Nill)
  }

  "`init` of Lizt(1, 2, 3)" should "be Lizt(1, 2)" in {
    val l123 = Lizt(1, 2, 3)
    val l12 = Lizt(1, 2)

    Lizt.init(l123) should be(l12)
  }
}

class Excercise3_9 extends AnyFlatSpec with Matchers {
  "Length of an empty lizt" should "be 0" in {
    Lizt.length(Nill) should be(0)
  }

  "Length of Lizt(1, 2, 3)" should "be 3" in {
    val l123 = Lizt(1, 2, 3)

    Lizt.length(l123) should be(3)
  }
}

class Excercise3_11 extends AnyFlatSpec with Matchers {
  "Sum with foldLeft" should "be 0 for empty lizts" in {
    Lizt.sumFoldl(Nill) should be(0)
  }

  it should "be the same as with foldRight" in {
    val l123 = Lizt(1, 2, 3)

    Lizt.sumFoldl(l123) should be(Lizt.sum2(l123))
  }

  "Product with foldLeft" should "be 1 for empty lizts" in {
    Lizt.productFoldl(Nill) should be(1)
  }

  it should "be the same as with foldRight" in {
    val l123d = Lizt(1.0, 2.0, 3.0)

    Lizt.productFoldl(l123d) should be(Lizt.product2(l123d))
  }

  "Length with foldLeft" should "be 0 for empty lizts " in {
    Lizt.lengthFoldl(Nill) should be(0)
  }

  it should "be the same as with foldRight" in {
    val l123 = Lizt(1, 2, 3)

    Lizt.lengthFoldl(l123) should be(Lizt.length(l123))
  }
}

class Excercise3_12 extends AnyFlatSpec with Matchers {
  "Reverse of an empty lizt" should "be itself(an empty lizt)" in {
    Lizt.reverse(Nill) should be(Nill)
  }

  "Reverse of a symmetric lizt" should "be itself" in {
    val l121 = Lizt(1, 2, 1)

    Lizt.reverse(l121) should be(l121)
  }

  "Reverse of Lizt(1, 2, 3)" should "be Lizt(3, 2, 1))" in {
    val l123 = Lizt(1, 2, 3)
    val l321 = Lizt(3, 2, 1)

    Lizt.reverse(l123) should be(l321)
  }
}

class Excercise3_13 extends AnyFlatSpec with Matchers {
  "Lizt creation with `classical` foldLeft" should "be equivalent to foldLeftViaFoldRight" in {
    val l123 = Lizt(1, 2, 3)
    val l321 = Lizt(3, 2, 1)

    Lizt.foldLeft(l123, Nill: Lizt[Int])((l, h) => Cons(h, l)) should be(l321)
    Lizt.foldLeft(l123, Nill: Lizt[Int])((l, h) => Cons(h, l)) should be(
      Lizt.foldLeftViaFoldRight(l123, Nill: Lizt[Int])((l, h) => Cons(h, l))
    )
  }

  "Lizt creation with `classical` foldRight" should "be equivalent to foldRightViaFoldLeft" in {
    val l123 = Lizt(1, 2, 3)

    Lizt.foldRight(l123, Nill: Lizt[Int])(Cons(_, _)) should be(
      Lizt.foldRightViaFoldLeft(l123, Nill: Lizt[Int])(Cons(_, _))
    )
  }
}

class Excercise3_14 extends AnyFlatSpec with Matchers {
  "Appending an empty lizt to an empty lizt" should "result in an empty lizt" in {
    Lizt.append(Nill, Nill) should be(Nill)
  }

  "Appending an empty lizt to a nempty lizt" should "result in the original non-empty lizt" in {
    val l123 = Lizt(1, 2, 3)

    Lizt.append(l123, Nill) should be(l123)
  }

  "Appending a non-empty lizt to an empty lizt" should "result in the original non-empty lizt" in {
    val l123 = Lizt(1, 2, 3)

    Lizt.append(Nill, l123) should be(l123)
  }

  "Appending Lizt(1, 2, 3) to Lizt(4, 5, 6)" should "result in Lizt(1, 2, 3, 4, 5, 6)" in {
    val l123 = Lizt(1, 2, 3)
    val l456 = Lizt(4, 5, 6)
    val l123456 = Lizt(1, 2, 3, 4, 5, 6)

    Lizt.append(l123, l456) should be(l123456)
  }
}

class Excercise3_15 extends AnyFlatSpec with Matchers {
  "Concatenation of a lizt of empty lizts" should "be an empty lizt" in {
    val loel = Lizt(Nill, Nill, Nill)

    Lizt.concat(loel) should be(Nill)
  }

  "Concatenation of Lizt(Lizt(1, 2, 3), Lizt(4, 5, 6))" should "be Lizt(1, 2, 3, 4, 5, 6)" in {
    val l123 = Lizt(1, 2, 3)
    val l456 = Lizt(4, 5, 6)
    val lol = Lizt(l123, l456)
    val l123456 = Lizt(1, 2, 3, 4, 5, 6)

    Lizt.concat(lol) should be(l123456)
  }

  "Concatenation of lizt of one non-empty lizt and other empty lizts" should "be the one non-empty lizt" in {
    val l123 = Lizt(1, 2, 3)
    val lol1 = Lizt(Nill, l123, Nill)
    val lol2 = Lizt(Nill, Nill, l123)
    val lol3 = Lizt(l123, Nill, Nill)

    Lizt.concat(lol1) should be(l123)
    Lizt.concat(lol2) should be(l123)
    Lizt.concat(lol3) should be(l123)
  }
}

class Excercise3_16 extends AnyFlatSpec with Matchers {
  "`plus1` over an empty lizt" should "be an empty list" in {
    Lizt.plus1(Nill) should be(Nill)
  }

  "`plus1` over Lizt(1, 2, 3)" should "be Lizt(2, 3, 4)" in {
    val l123 = Lizt(1, 2, 3)
    val l234 = Lizt(2, 3, 4)

    Lizt.plus1(l123) should be(l234)
  }
}

class Excercise3_17 extends AnyFlatSpec with Matchers {
  "`stringifyDoubles` over an empty lizt" should "be an empty lizt" in {
    Lizt.stringifyDoubles(Nill) should be(Nill)
  }

  "`stringifyDoubles` over Lizt(1.0, 2.0, 3.0)" should "be Lizt(\"1.0\", \"2.0\", \"3.0\")" in {
    val l123d = Lizt(1.0, 2.0, 3.0)
    val l123ds = Lizt("1.0", "2.0", "3.0")

    Lizt.stringifyDoubles(l123d) should be(l123ds)
  }
}

class Excercise3_18 extends AnyFlatSpec with Matchers {
  "`map` with adding one over a lizt of integers" should "behave as `plus1`" in {
    val l123 = Lizt(1, 2, 3)

    Lizt.map(l123)(_ + 1) should be(Lizt.plus1(l123))
  }

  "`map` with stringifying doubles over a lizt of doubles" should "behave as `stringifyDoubles`" in {
    val l123d = Lizt(1.0, 2.0, 3.0)

    Lizt.map(l123d)(_.toString) should be(Lizt.stringifyDoubles(l123d))
  }
}

class Excercise3_19 extends AnyFlatSpec with Matchers {
  "A `filter` over an empty lizt" should "be an empty lizt with a true filter" in {
    val allwaysTrue = (_: Nothing) => true

    Lizt.filter(Nill)(allwaysTrue) should be(Nill)
  }

  it should "be an empty lizt with a false filter" in {
    val allwaysFalse = (_: Nothing) => false

    Lizt.filter(Nill)(allwaysFalse) should be(Nill)
  }

  "A `filter` over a non-empty lizt" should "be the same lizt with a true filter" in {
    val l123 = Lizt(1, 2, 3)
    val allwaysTrue = (_: Int) => true

    Lizt.filter(l123)(allwaysTrue) should be(l123)
  }

  it should "be an empty lizt with a false filter" in {
    val l123 = Lizt(1, 2, 3)
    val allwaysFalse = (_: Int) => false

    Lizt.filter(l123)(allwaysFalse) should be(Nill)
  }

  "A `filter` over Lizt(1, 2, 3)" should "be Lizt(1, 3) with an odd filer" in {
    val isOdd = (n: Int) => (n % 2) != 0
    val l123 = Lizt(1, 2, 3)

    Lizt.filter(l123)(isOdd) should be(Lizt(1, 3))
  }

  it should "be Lizt(2) with an even filer" in {
    val isEven = (n: Int) => (n % 2) == 0
    val l123 = Lizt(1, 2, 3)

    Lizt.filter(l123)(isEven) should be(Lizt(2))
  }

  "A `filter` over all evens lizt" should "be the same lizt with an even filer" in {
    val isEven = (n: Int) => (n % 2) == 0
    val allEven = Lizt(4, 2, 6)

    Lizt.filter(allEven)(isEven) should be(allEven)
  }

  "A `filter` over all odd lizt" should "be the same lizt with an odd filer" in {
    val isOdd = (n: Int) => (n % 2) != 0
    val allOdd = Lizt(1, 5, 3)

    Lizt.filter(allOdd)(isOdd) should be(allOdd)
  }
}

class Excercise3_20 extends AnyFlatSpec with Matchers {
  "`flatMap` over an empty lizt" should "be an empty lizt" in {
    Lizt.flatMap(Nill)(Lizt(_)) should be(Nill)
  }

  "`flatMap` over Lizt(1, 2, 3) with Lizt(i, i)" should "be Lizt(1, 1, 2, 2, 3, 3)" in {
    val l123 = Lizt(1, 2, 3)
    val l112233 = Lizt(1, 1, 2, 2, 3, 3)

    Lizt.flatMap(l123)(e => Lizt(e, e)) should be(l112233)
  }
}

class Excercise3_21 extends AnyFlatSpec with Matchers {
  "A `filterViaFlatMap` over an empty lizt" should "be an empty lizt with a true filter" in {
    val allwaysTrue = (_: Nothing) => true

    Lizt.filterViaFlatMap(Nill)(allwaysTrue) should be(Nill)
  }

  it should "be an empty lizt with a false filter" in {
    val allwaysFalse = (_: Nothing) => false

    Lizt.filterViaFlatMap(Nill)(allwaysFalse) should be(Nill)
  }

  "A `filterViaFlatMap` over a non-empty lizt" should "be the same lizt with a true filter" in {
    val l123 = Lizt(1, 2, 3)
    val allwaysTrue = (_: Int) => true

    Lizt.filterViaFlatMap(l123)(allwaysTrue) should be(l123)
  }

  it should "be an empty lizt with a false filter" in {
    val l123 = Lizt(1, 2, 3)
    val allwaysFalse = (_: Int) => false

    Lizt.filterViaFlatMap(l123)(allwaysFalse) should be(Nill)
  }

  "A `filterViaFlatMap` over Lizt(1, 2, 3)" should "be Lizt(1, 3) with an odd filer" in {
    val isOdd = (n: Int) => (n % 2) != 0
    val l123 = Lizt(1, 2, 3)

    Lizt.filterViaFlatMap(l123)(isOdd) should be(Lizt(1, 3))
  }

  it should "be Lizt(2) with an even filer" in {
    val isEven = (n: Int) => (n % 2) == 0
    val l123 = Lizt(1, 2, 3)

    Lizt.filterViaFlatMap(l123)(isEven) should be(Lizt(2))
  }

  "A `filterViaFlatMap` over all evens lizt" should "be the same lizt with an even filer" in {
    val isEven = (n: Int) => (n % 2) == 0
    val allEven = Lizt(4, 2, 6)

    Lizt.filterViaFlatMap(allEven)(isEven) should be(allEven)
  }

  "A `filterViaFlatMap` over all odd lizt" should "be the same lizt with an odd filer" in {
    val isOdd = (n: Int) => (n % 2) != 0
    val allOdd = Lizt(1, 5, 3)

    Lizt.filterViaFlatMap(allOdd)(isOdd) should be(allOdd)
  }
}

class Excercise3_22 extends AnyFlatSpec with Matchers {
  "`intLiztAdder` with Lizt(1, 2, 3) and Lizt(4, 5, 6)" should "be Lizt(5, 7, 9)" in {
    val l123 = Lizt(1, 2, 3)
    val l456 = Lizt(4, 5, 6)
    val l579 = Lizt(5, 7, 9)

    Lizt.intLiztAdder(l123, l456) should be(l579)
  }

  "`intLiztAdder` with any or both of the lizts being empty" should "result in empty lizt" in {
    val l123 = Lizt(1, 2, 3)

    Lizt.intLiztAdder(l123, Nill) should be(Nill)
    Lizt.intLiztAdder(Nill, l123) should be(Nill)
    Lizt.intLiztAdder(Nill, Nill) should be(Nill)
  }

  "`intLiztAdder` result" should "be the length of the shorter lizt" in {
    val l12 = Lizt(1, 2)
    val l12345 = Lizt(1, 2, 3, 4, 5)

    Lizt.length(Lizt.intLiztAdder(l12, l12345)) should be(Lizt.length(l12))
  }
}

class Excercise3_23 extends AnyFlatSpec with Matchers {
  "`zipWith` with Lizt(1, 2, 3) and Lizt(4, 5, 6)" should "be Lizt(5, 7, 9) with addition as the zipping function" in {
    val l123 = Lizt(1, 2, 3)
    val l456 = Lizt(4, 5, 6)
    val l579 = Lizt(5, 7, 9)

    Lizt.zipWith(l123, l456)(_ + _) should be(l579)
  }

  "`zipWith` with Lizt(1, 2, 3) and Lizt(4, 5, 6)" should "be Lizt(5, 7, 9) with subtraction as the zipping function" in {
    val l123 = Lizt(1, 2, 3)
    val l456 = Lizt(4, 5, 6)
    val lm3m3m3 = Lizt(-3, -3, -3)

    Lizt.zipWith(l123, l456)(_ - _) should be(lm3m3m3)
  }

  "`zipWith` with any or both of the lizts being empty" should "result in empty lizt regardles of the zipping function" in {
    val l123 = Lizt(1, 2, 3)

    Lizt.zipWith(l123, Nill)(_ + _) should be(Nill)
    Lizt.zipWith(Nill, l123)(_ + _) should be(Nill)
    Lizt.zipWith(Nill: Lizt[Int], Nill: Lizt[Int])(_ + _) should be(Nill)
    Lizt.zipWith(l123, Nill)(_ - _) should be(Nill)
    Lizt.zipWith(Nill, l123)(_ - _) should be(Nill)
    Lizt.zipWith(Nill: Lizt[Int], Nill: Lizt[Int])(_ - _) should be(Nill)
  }

  "`zipWith` result" should "be the length of the shorter lizt regardles of the zipping function" in {
    val l12 = Lizt(1, 2)
    val l12345 = Lizt(1, 2, 3, 4, 5)

    Lizt.length(Lizt.zipWith(l12, l12345)(_ + _)) should be(Lizt.length(l12))
    Lizt.length(Lizt.zipWith(l12, l12345)(_ - _)) should be(Lizt.length(l12))
  }
}

class Excercise3_24 extends AnyFlatSpec with Matchers {
  val l123 = Lizt(1, 2, 3)
  val l1 = Lizt(1)
  val l12 = Lizt(1, 2)
  val l32 = Lizt(3, 2)
  val labc = Lizt('a', 'b', 'c')

  "Empty lizt" should "be a subsequence of any lizt" in {
    Lizt.hasSubsequence(Nill, Nill) should be(true)
    Lizt.hasSubsequence(l123, Nill) should be(true)
    Lizt.hasSubsequence(labc, Nill) should be(true)
  }

  "Empty lizt" should "have only one subsequence i.e. an empty lizt" in {
    Lizt.hasSubsequence(Nill, Nill) should be(true)
    Lizt.hasSubsequence(Nill, l123) should be(false)
    Lizt.hasSubsequence(Nill, labc) should be(false)
  }

  "Lizt(1)" should "be a subsequence to Lizt(1, 2, 3)" in {
    Lizt.hasSubsequence(l123, l1) should be(true)
  }

  "Lizt(1, 2)" should "be a subsequence to Lizt(1, 2, 3)" in {
    Lizt.hasSubsequence(l123, l12) should be(true)
  }

  "Lizt(3, 2)" should "not be a subsequence to Lizt(1, 2, 3 )" in {
    Lizt.hasSubsequence(l123, l32) should be(false)
  }

  "Lizt(1, 2)" should "be a subsequence to Lizt(1, 1, 2, 3)" in {
    val l1123 = Lizt(1, 1, 2, 3)

    Lizt.hasSubsequence(l1123, l12) should be(true)
  }
}
