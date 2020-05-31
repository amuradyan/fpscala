package fpinscala
package chapter5
package tests

import fpinscala.chapter4.opshn.Opshn
import fpinscala.chapter4.opshn.Non
import fpinscala.chapter4.opshn.Sam
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

class Excercise5_4 extends AnyFlatSpec with Matchers {
  val isOdd = (n: Int) => (n % 2) != 0

  "`forAll` with any predicate over an Emptie" should "be FALSE" in {
    Emptie.forAll(_ => true) should be (true)
    Emptie.forAll(_ => false) should be (true)
  }

  "`forAll` with and odd predicate over Strim(1, 3, 5)" should "TRUE" in {
    Strim(1, 3, 5).forAll(isOdd) should be (true)
  }

  "`forAll` with and even predicate over Strim(2, 4, 6)" should "FALSE" in {
    Strim(2, 4, 6).forAll(isOdd) should be (false)
  }
}

class Excercise5_5 extends AnyFlatSpec with Matchers {
  val s1234 = Strim(1, 2, 3, 4)
  val sABCD = Strim('A', 'B', 'C', 'D')

  "`takeWhileViaFoldRight` over any Strim with a FALSE predicate" should "allways be Emptie" in {
    s1234.takeWhileViaFoldRight(_ => false) should be (Emptie)
    sABCD.takeWhileViaFoldRight(_ => false) should be (Emptie)
  }

  "`takeWhileViaFoldRight` over any Strim with a TRUE predicate" should "allways be the original Strim" in {
    s1234.takeWhileViaFoldRight(_ => true).toLizt should be (Lizt(1, 2, 3, 4))
    sABCD.takeWhileViaFoldRight(_ => true).toLizt should be (Lizt('A', 'B', 'C', 'D'))
  }

  "`takeWhileViaFoldRight` over Strim(1, 2, 3, 4) with an odd predicate" should "be Strim(1, 3)" in {
    val isOdd = (n: Int) => (n % 2) != 0
    s1234.takeWhileViaFoldRight(isOdd).toLizt should be ((Lizt(1, 3)))
  }

  "`takeWhileViaFoldRight` over Strim(1, 2, 3, 4) with an even predicate" should "be Strim(2, 4)" in {
    val isEven = (n: Int) => (n % 2) == 0
    s1234.takeWhileViaFoldRight(isEven).toLizt should be ((Lizt(2, 4)))
  }

  "`takeWhileViaFoldRight` over Emptie with ANY predicate" should "be Emptie" in {
    Emptie.takeWhileViaFoldRight(_ => true) should be (Emptie)
    Emptie.takeWhileViaFoldRight(_ => false) should be (Emptie)
  }
}

class Excercise5_6 extends AnyFlatSpec with Matchers {
  "`headOpshnViaFoldRight` over Emptie" should "be Non" in {
    Emptie.headOpshnViaFoldRight should be (Non)
  }

  "`headOpshnViaFoldRight` over Strim(1, 2)" should "be Sam(1)" in {
    Strim(1, 2).headOpshnViaFoldRight should be (Sam(1))
  }

  "`headOpshnViaFoldRight` over Strim(1)" should "be Sam(1)" in {
    Strim(1).headOpshnViaFoldRight should be (Sam(1))
  }
}

class Excercise5_7 extends AnyFlatSpec with Matchers {
  "`map` over Emptie" should "be Emptie" in {
    Emptie.map(_ => 1) should be (Emptie)
  }

  "`map` over Strim(4, 9, 16) with square rooting" should "be Strim(2, 3, 4)" in {
    Strim(4, 9, 16).map(math.sqrt(_)).toLizt should be (Strim(2, 3, 4).toLizt)
  }

  "`flatMap` over Emptie" should "be Emptie" in {
    Emptie.flatMap(a => Strim(a)) should be (Emptie)
  }

  "`flatMap` over Strim(4, 9, 16) with square rooting" should "be Strim(2, 3, 4)" in {
    Strim(4, 9, 16).flatMap(a => Strim(math.sqrt(a))).toLizt should be (Strim(2, 3, 4).toLizt)
  }

  "`filtering` Emptie with ANY filter" should "result in Emptie" in {
    Emptie.filter(_ => false) should be (Emptie)
    Emptie.filter(_ => true) should be (Emptie)
  }

  "`filtering` Strim(1, 2, 3, 4) with an odd filter" should "produce Strim(2, 4)" in {
    val isOdd = (n: Int) => (n % 2) != 0
    Strim(1, 2, 3, 4).filter(isOdd).toLizt should be (Strim(1, 3).toLizt)
  }

  "`filtering` Strim(1, 2, 3, 4) with an even filter" should "produce Strim(1, 3)" in {
    val isEven = (n: Int) => (n % 2) == 0
    Strim(1, 2, 3, 4).filter(isEven).toLizt should be (Strim(2, 4).toLizt)
  }

  "appending Emptie to Emptie" should "result in Emptie" in {
    Emptie.append(Emptie) should be (Emptie)
  }

  "appending Emptie to Strim" should "result in the original Strim" in {
    Strim(1, 2, 3).append(Emptie).toLizt should be (Strim(1, 2, 3).toLizt)
  }

  "appending a Strim to Emptie" should "result in appended Strim" in {
    Emptie.append(Strim(1, 2, 3)).toLizt should be (Strim(1, 2, 3).toLizt)
  }

  "appending Strim(1, 2) to Strim(3, 4)" should "result in Strim(1, 2, 3, 4)" in {
    Strim(1, 2).append(Strim(3, 4)).toLiztRec should be (Strim(1, 2, 3, 4).toLizt)
  }
}

class Excercise5_8 extends AnyFlatSpec with Matchers {
  "`constant` Strim of a number" should "constantly generate that number" in {
    val a = Random.nextInt(30)
    val b = Random.nextInt(30)
    val c = Random.nextInt(30)
    val v = 1

    val l = List.fill(a)(1)
    Strim.constant(v).take(a).toLizt should be (Lizt.fill(a)(v))
    Strim.constant(v).take(b).toLizt should be (Lizt.fill(b)(v))
    Strim.constant(v).take(c).toLizt should be (Lizt.fill(c)(v))
  }
}

class Excercise5_9 extends AnyFlatSpec with Matchers {
  "seventh element `from` 1" should "be 7" in {
    Strim.from(1).drop(6).take(1).headOpshn should be (Sam(7))
  }

  "first element `from` 1" should "be 1" in {
    Strim.from(1).take(1).headOpshn should be (Sam(1))
  }
}

class Excercise5_10 extends AnyFlatSpec with Matchers {
  "A Fibonacci Strim" should "return 0 for 0th take" in {
    Strim.fibs.take(1).headOpshn should be (Sam(0));
  }

  it should "return 1 for 2nd take" in {
    Strim.fibs.drop(1).take(1).headOpshn should be (Sam(1));
  }

  it should "return 1 for 3rd take" in {
    Strim.fibs.drop(2).take(1).headOpshn should be (Sam(1));
  }

  it should "return 2 for 4th take" in {
    Strim.fibs.drop(3).take(1).headOpshn should be (Sam(2));
  }

  it should "return 34 for 10th take" in {
    Strim.fibs.drop(9).take(1).headOpshn should be (Sam(34))
  }
}

class Excercise5_11 extends AnyFlatSpec with Matchers {
  import Strim._

  def ASCIIfy(s: String): Opshn[(Int, String)] = s match {
    case "" => Non
    case s  => Sam(s.head.toInt, s.tail)
  }


  "`unfold`-ing anything with Non" should "return Emptie" in {
    Strim.unfold(0)(a => Non) should be (Emptie)
    Strim.unfold("0")(a => Non) should be (Emptie)
    Strim.unfold(false)(a => Non) should be (Emptie)
  }

  "`unfold`-ing a string via ASCII-fier" should "retun the Lizt of ASCII codes fo the string" in {
    val source = "Raphique"
    val ASCIIfiedRafique = Lizt(82, 97, 112, 104, 105, 113, 117, 101)

    val s = Strim.unfold(source) { ASCIIfy }

    s.take(8).toLizt should be (ASCIIfiedRafique)
  }

  "9th element of `unfold`-ing a 'Raphique' via ASCII-fier" should "retun Nill" in {
    val source = "Raphique"

    val s = Strim.unfold(source) { ASCIIfy }

    s.drop(8).take(1).toLizt should be (Nill)
  }
}

class Excercise5_12 extends AnyFlatSpec with Matchers {
  import Strim._

  "`fibsViaUnfold`" should "behave as `fibs`" in {
    Strim.fibs.take(1).headOpshn should be (Strim.fibsViaUnfold.take(1).headOpshn)
    Strim.fibs.drop(2).take(1).headOpshn should be (Strim.fibsViaUnfold.drop(2).take(1).headOpshn)
    Strim.fibs.drop(3).take(1).headOpshn should be (Strim.fibsViaUnfold.drop(3).take(1).headOpshn)
    Strim.fibs.drop(4).take(1).headOpshn should be (Strim.fibsViaUnfold.drop(4).take(1).headOpshn)
    Strim.fibs.drop(10).take(1).headOpshn should be (Strim.fibsViaUnfold.drop(10).take(1).headOpshn)
  }

  "`fromViaUnfold`" should "behave as `from`" in {
    Strim.fromViaUnfold(1).take(1).headOpshn should be (Strim.from(1).take(1).headOpshn)
    Strim.fromViaUnfold(1).drop(6).take(1).headOpshn should be (Strim.from(1).drop(6).take(1).headOpshn)
  }

  "`constantViaUnfold`" should "behave as `constant`" in {
    val a = Random.nextInt(30)
    val b = Random.nextInt(30)
    val c = Random.nextInt(30)
    val v = 1

    val l = List.fill(a)(1)
    Strim.constant(v).take(a).toLizt should be (Strim.constantViaUnfold(v).take(a).toLizt)
    Strim.constant(v).take(b).toLizt should be (Strim.constantViaUnfold(v).take(b).toLizt)
    Strim.constant(v).take(c).toLizt should be (Strim.constantViaUnfold(v).take(c).toLizt)
  }

  "`onesViaUnfold`" should "behave as `ones`" in {
    val idx = Random.nextInt(100)
    
    Strim.onesViaUnfold.drop(idx).take(1).headOpshn should be (Sam(1))
    Strim.onesViaUnfold.drop(idx).take(1).toLizt should be (Strim.ones.drop(idx).take(1).toLizt)
  }
}
