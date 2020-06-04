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

  "`takeWhile` over Strim(1, 2, 3, 4) with an odd predicate" should "be Strim(1)" in {
    val isOdd = (n: Int) => (n % 2) != 0
    s1234.takeWhile(isOdd).toLizt should be ((Lizt(1)))
  }

  "`takeWhile` over Strim(1, 2, 3, 4) with an even predicate" should "be Emptie" in {
    val isEven = (n: Int) => (n % 2) == 0
    s1234.takeWhile(isEven).toLizt should be (Nill)
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

  "`unfold`-ing Emptie via anything" should "be Emptie" in {
    Strim.unfold(0)(_ => Non) should be (Emptie) 
    Strim.unfold(true)(_ => Non) should be (Emptie) 
    Strim.unfold("")(_ => Non) should be (Emptie) 
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

class Excercise5_13 extends AnyFlatSpec with Matchers {
  import Strim._

  val s12345 = Strim(1, 2, 3, 4, 5)
  val s1234 = Strim(1, 2, 3, 4)
  val sABCD = Strim('A', 'B', 'C', 'D')

  "`mapViaUnfold` over Emptie" should "behave as `map`" in {
    Emptie.mapViaUnfold(_ => 1) should be (Emptie.map(_ => 1))
  }

  "`mapViaUnfold` over Strim(4, 9, 16) with square rooting" should "behave as `map`" in {
    Strim(4, 9, 16).mapViaUnfold(math.sqrt(_)).toLizt should be (Strim(4, 9, 16).map(math.sqrt(_)).toLizt)
  }

  "`takeViaUnfold`-ing from an Emptie" should "behave as `take`-ing" in {
    val idx = Random.nextInt()
    Emptie.takeViaUnfold(idx) should be (Emptie.take(idx))
  }

  "`takeViaUnfold`-ing 4 over Strim(1, 2, 3, 4, 5)" should "behave as `take`-ing" in {
    s12345.takeViaUnfold(4).toLizt should be (s12345.take(4).toLizt)
  }

  "`takeViaUnfold`-ing all from a Strim" should "behave as `take`-ing" in {
    s12345.takeViaUnfold(5).toLizt should be (s12345.take(5).toLizt)
  }

  "`takeViaUnfold`-ing more than the original Strim" should "behave as `take`-ing" in {
    s12345.takeViaUnfold(10).toLizt should be (s12345.take(10).toLizt)
  }

  "`takeWhileViaUnfold` over any Strim with a FALSE predicate" should "behave as `takeWhile`" in {
    s1234.takeWhileViaUnfold(_ => false) should be (s1234.takeWhile(_ => false))
    sABCD.takeWhileViaUnfold(_ => false) should be (sABCD.takeWhile(_ => false))
  }

  "`takeWhileViaUnfold` over any Strim with a TRUE predicate" should "behave as `takeWhile`" in {
    s1234.takeWhileViaFoldRight(_ => true).toLizt should be (s1234.takeWhile(_ => true).toLizt)
    sABCD.takeWhileViaFoldRight(_ => true).toLizt should be (sABCD.takeWhile(_ => true).toLizt)
  }

  "`takeWhileViaUnfold` over Strim(1, 2, 3, 4) with an odd predicate" should "behave as `takeWhile`" in {
    val isOdd = (n: Int) => (n % 2) != 0
    s1234.takeWhileViaUnfold(isOdd).toLizt should be (s1234.takeWhile(isOdd).toLizt)
  }

  "`takeWhileViaUnfold` over Strim(1, 2, 3, 4) with an even predicate" should "behave as `takeWhile`" in {
    val isEven = (n: Int) => (n % 2) == 0
    s1234.takeWhileViaUnfold(isEven).toLizt should be (s1234.takeWhile(isEven).toLizt)
  }

  "`takeWhileViaUnfold` over Emptie with ANY predicate" should "behave as `takeWhile`" in {
    Emptie.takeWhileViaUnfold(_ => true) should be (Emptie.takeWhile(_ => true))
    Emptie.takeWhileViaUnfold(_ => false) should be (Emptie.takeWhile(_ => false))
  }

  "`zipWith` an Emptie" should "return Emptie" in {
    val s123 = Strim(1, 2, 3)

    s123.zipWith(Emptie)(_ + _) should be (Emptie)
    emptie[String].zipWith(s123)(_ + _) should be (Emptie)
  }

  "`zipWith` with Strim(1, 2, 3) and Strim(4, 5, 6)" should "be Strim(5, 7, 9) with addition as the zipping function" in {
    val s123 = Strim(1, 2, 3)
    val s456 = Strim(4, 5, 6)
    val l579 = Lizt(5, 7, 9)

    s123.zipWith(s456)(_ + _).toLizt should be (l579)
  }

  "`zipWith` with Strim(1, 2, 3) and Strim(4, 5, 6)" should "be Strim(-3, -3, -3) with subtraction as the zipping function" in {
    val s123 = Strim(1, 2, 3)
    val s456 = Strim(4, 5, 6)
    val lm3m3m3 = Lizt(-3, -3, -3)

    s123.zipWith(s456)(_ - _).toLizt should be (lm3m3m3)
  }

  "`zipWith` with any or both of the Strims being Emptie" should "result in Emptie regardles of the zipping function" in {
    val s123 = Strim(1, 2, 3)

    s123.zipWith(emptie[Int])(_ + _) should be (Emptie)
    emptie[Int].zipWith(s123)(_ + _) should be (Emptie)
    emptie[Int].zipWith(emptie[Int])(_ + _) should be (Emptie)
    s123.zipWith(emptie[Int])(_ - _) should be (Emptie)
    emptie[Int].zipWith(s123)(_ - _) should be (Emptie)
    emptie[Int].zipWith(emptie[Int])(_ - _) should be (Emptie)
  }

  "`zipWith` result" should "be the length of the shorter Strim regardles of the zipping function" in {
    val s12 = Strim(1, 2)
    val s12345 = Strim(1, 2, 3, 4, 5)
    val l12 = Lizt(1, 2)

    Lizt.length(s12.zipWith(s12345)(_ + _).toLizt) should be (Lizt.length(l12))
    Lizt.length(s12.zipWith(s12345)(_ - _).toLizt) should be (Lizt.length(l12))
  }

  "`zipAll` an Emptie with a Strim" should "return the Strim" in {
    val s123 = Strim(1, 2, 3)
    val strimEmptieZip = Lizt((Sam(1), Non), (Sam(2), Non), (Sam(3), Non))
    val emptieStrimZip = Lizt((Non, Sam(1)), (Non, Sam(2)), (Non, Sam(3)))

    s123.zipAll(Emptie).toLizt should be (strimEmptieZip)
    emptie[String].zipAll(s123).toLizt should be (emptieStrimZip)
  }

  "`zipAll` with Strim(1, 2, 3) and Strim(4, 5, 6)" should "be Lizt((Sam(1), Sam(4)), (Sam(2), Sam(5)), (Sam(3), Sam(6)))" in {
    val s123 = Strim(1, 2, 3)
    val s456 = Strim(4, 5, 6)
    val strimStrimZip = Lizt((Sam(1), Sam(4)), (Sam(2), Sam(5)), (Sam(3), Sam(6)))

    s123.zipAll(s456).toLizt should be (strimStrimZip)
  }

  "`zipAll` with two Strims" should "should stay productive, while any of the strims is productive" in {
    val s123 = Strim(1, 2, 3)
    val s12 = Strim(1, 2)
    val s12s123Zip = Lizt((Sam(1), Sam(1)), (Sam(2), Sam(2)), (Non, Sam(3)))
    val s123s12Zip = Lizt((Sam(1), Sam(1)), (Sam(2), Sam(2)), (Sam(3), Non))

    s12.zipAll(s123).toLizt should be (s12s123Zip)
    s123.zipAll(s12).toLizt should be (s123s12Zip)
  }
}

class Excercise5_14 extends AnyFlatSpec with Matchers {
  import Strim._

  "Strim(1, 2, 3)" should "`startWith` Strim (1, 2)" in {
    val s123 = Strim(1, 2, 3)
    val s12 = Strim(1, 2)
    
    s123 startsWith s12 should be (true)
  }
  
  it should "`startWith` Strim (1, 2, 3)" in {
    val s123 = Strim(1, 2, 3)
    
    s123 startsWith s123 should be (true)
  }
  
  it should "not `startWith` Strim (1, 2, 4)" in {
    val s123 = Strim(1, 2, 3)
    val s124 = Strim(1, 2, 4)
    
    s123 startsWith s124 should be (false)
  }
  
  "Strim(1, 2, 4)" should "not `startWith` Strim (1, 2, 3, 4)" in {
    val s1234 = Strim(1, 2, 3, 4)
    val s124 = Strim(1, 2, 4)

    s124 startsWith s1234 should be (false)
  }

  "Strim(1, 2, 4, 4)" should "not `startWith` Strim (1, 2, 3, 4)" in {
    val s1234 = Strim(1, 2, 3, 4)
    val s1244 = Strim(1, 2, 4, 4)

    s1244 startsWith s1234 should be (false)
  }

  "Emptie" can "not `startsWith` anything" in {
    val s123 = Strim(1, 2, 3)
    val s12 = Strim(1, 2)

    Emptie startsWith s123 should be (false)
    Emptie startsWith s12 should be (false)
  }

  "Nothing" can "`startWith` Emptie" in {
    val s123 = Strim(1, 2, 3)
    val s12 = Strim(1, 2)

    s12 startsWith Emptie should be (false)
    s123 startsWith Emptie should be (false)
  }

  "Emptie" can "not `startWith` itself" in {
    Emptie startsWith Emptie should be (false)
  }
}

class Excercise5_15 extends AnyFlatSpec with Matchers {
  "`tails` of Strim(1, 2, 3)" should "be Strim(Strim(1, 2, 3), Strim(1, 2), Strim(1), Strim())" in {
      val s123 = Strim(1, 2, 3)
      val tailsOfs123 = Lizt(Lizt(1, 2, 3), Lizt(2, 3), Lizt(3))

      s123.tails.map( _.toLizt ).toLizt should be (tailsOfs123)
    }

  "`tails` of Emptie" should "be Emptie" in {
    Emptie.tails should be (Emptie)
  }
}

class Excercise5_16 extends AnyFlatSpec with Matchers {
  "`scanRight` over Emptie" should "be the zero element be Emptie" in {
    Emptie.scanRight(0){(a, b) => 1}.toLizt should be (Lizt(0))
    Emptie.scanRight("")((a, b) => "ab").toLizt should be (Lizt(""))
    Emptie.scanRight(Emptie){(a, b) => Emptie}.toLizt should be (Lizt(Emptie))
  }

  "`scanRight` over Strim(1, 2, 3) with adder" should "be Strim(6, 5, 3, 0)" in {
    Strim(1, 2, 3).scanRight(0)(_ + _).toLizt should be (Lizt(6, 5, 3, 0))
  }

  "`scanRight` over Strim('a', 'b', 'c') with adder" should "be Strim('abc', 'bc', 'c', '')" in {
    Strim("a", "b", "c").scanRight("")(_ + _).toLizt should be (Lizt("abc", "bc", "c", ""))
  }
}

class Misc extends AnyFlatSpec with Matchers {
  "Strim(3, 4)" should "be a subsequence of Strim(2, 3, 4, 5)" in {
    Strim(2, 3, 4, 5) hasSubsequence Strim(3, 4) should be (true)
  }

  "Strim(3, 4)" should "not be a subsequence of Strim(2, 3, 2, 3)" in {
    Strim(2, 3, 2, 3) hasSubsequence Strim(3, 4) should be (false)
  }

  "Emptie" should "not be a subsequence of anything" in {
    Strim(1, 2, 3) hasSubsequence Emptie should be (false)
    Strim() hasSubsequence Emptie should be (false)
    Emptie hasSubsequence Emptie should be (false)
  }

  "Nothing" should "be a subsequence of Emptie" in {
    Emptie hasSubsequence Strim(1, 2, 3) should be (false)
    Emptie hasSubsequence Strim() should be (false)
    Emptie hasSubsequence Emptie should be (false)
  }
}