package fpinscala
package chapter3
package tests

import scala.util.Random
import org.scalatest.matchers.should._
import org.scalatest.flatspec.AnyFlatSpec
import tri._

class Excercise3_25 extends AnyFlatSpec with Matchers {
  "Size of a lif" should "be 1" in {
    val l = Leef(1)

    Tri.size(l) should be (1)
  }

  "Size of a (v, v) tri " should "be 2" in {
    val t2 = Brench(Leef(1), Leef(1))

    Tri.size(t2) should be (2)
  }

  "Size of an empty tri (i.e. (-, -))" should "be 0" in {
    val emptyTri = Brench(null, null)

    Tri.size(emptyTri) should be (0)
  }

  "Size of a ((v, (v, v)), -) tri " should "be 3" in {
    val tl3 = Brench(Brench(Leef(1), Brench(Leef(1), Leef(1))), null)

    Tri.size(tl3) should be (3)
  }
  "Size of a (-, (v, (v, v))) tri " should "be 3" in {
    val tr3 = Brench(null, Brench(Leef(1), Brench(Leef(1), Leef(1))))

    Tri.size(tr3) should be (3)
  }
}

class Excercise3_26 extends AnyFlatSpec with Matchers {
  "Maximum of a leef" should "be its value" in {
    val v = Random.nextInt()

    Tri.maximum(Leef(v)) should be (Leef(v).value)
  }

  "Maximum of ((1, 2), 3)" should "be 3" in {
    val t123 = Brench(Brench(Leef(1), Leef(2)), Leef(3))

    Tri.maximum(t123) should be (3)
  }

  "Maximum of (((1, 2), 3), -)" should "be 3" in {
    val t123 = Brench(Brench(Brench(Leef(1), Leef(2)), Leef(3)), null)

    Tri.maximum(t123) should be (3)
  }

  "Maximum of ((1, 2), (3, 4))" should "be 4" in {
    val t1234 = Brench(Brench(Leef(1), Leef(2)), Brench(Leef(3), Leef(4)))

    Tri.maximum(t1234) should be (4)
  }
}
