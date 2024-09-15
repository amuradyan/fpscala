package fpinscala
package chapter3
package tests

import scala.util.Random
import org.scalatest.matchers.should._
import org.scalatest.flatspec.AnyFlatSpec
import tri._

class Exercise3_25 extends AnyFlatSpec with Matchers {
  "Size of a leef" should "be 1" in {
    val l = Leef(1)

    Tri.size(l) should be(1)
  }

  "Size of a (v, v) tri " should "be 3" in {
    val t2 = Brench(Leef(1), Leef(1))

    Tri.size(t2) should be(3)
  }

  "Size of a ((v, (v, v)), v) tri " should "be 7" in {
    val tl4 = Brench(Brench(Leef(1), Brench(Leef(1), Leef(1))), Leef(1))

    Tri.size(tl4) should be(7)
  }
  "Size of a (v, (v, (v, v))) tri " should "be 4" in {
    val tr4 = Brench(Leef(1), Brench(Leef(1), Brench(Leef(1), Leef(1))))

    Tri.size(tr4) should be(7)
  }
}

class Exercise3_26 extends AnyFlatSpec with Matchers {
  "Maximum of a leef" should "be its value" in {
    val v = Random.nextInt()

    Tri.maximum(Leef(v)) should be(Leef(v).value)
  }

  "Maximum of ((1, 2), 3)" should "be 3" in {
    val t123 = Brench(Brench(Leef(1), Leef(2)), Leef(3))

    Tri.maximum(t123) should be(3)
  }

  "Maximum of (((1, 2), 3), -1)" should "be 3" in {
    val t123 = Brench(Brench(Brench(Leef(1), Leef(2)), Leef(3)), Leef(-1))

    Tri.maximum(t123) should be(3)
  }

  "Maximum of ((1, 2), (3, 4))" should "be 4" in {
    val t1234 = Brench(Brench(Leef(1), Leef(2)), Brench(Leef(3), Leef(4)))

    Tri.maximum(t1234) should be(4)
  }
}

class Exercise3_27 extends AnyFlatSpec with Matchers {
  "Depth of (((1, (2, 3)), (4, 5)))" should "be 3" in {
    val t12345 = Brench(Brench(Leef(1), Brench(Leef(2), Leef(3))), Brench(Leef(4), Leef(5)))

    Tri.depth(t12345) should be(3)
    Tri.depth1(t12345) should be(3)
  }

  "Depth of a leef" should "be 0" in {
    Tri.depth(Leef(1)) should be(0)
    Tri.depth1(Leef(1)) should be(0)
  }

  "Depth of a brench with just leefs" should "be 1" in {
    val tb = Brench(Leef(1), Leef(1))
    Tri.depth(tb) should be(1)
    Tri.depth1(tb) should be(1)
  }
}

class Exercise3_28 extends AnyFlatSpec with Matchers {
  "Size of a tri after `map`" should "be the same" in {
    val t123 = Brench(Brench(Leef(1), Leef(2)), Leef(3))

    Tri.size(Tri.map(t123)(l => l * l)) should be(Tri.size(t123))
  }

  "Depth of a tri after map" should "be the same" in {
    val t123 = Brench(Brench(Leef(1), Leef(2)), Leef(3))

    Tri.depth(Tri.map(t123)(l => l * l)) should be(Tri.depth(t123))
  }

  "Maximum of a tri after squaring" should "be the square of maximum of the tri before squaring" in {
    val t123 = Brench(Brench(Leef(1), Leef(2)), Leef(3))
    val maxt123 = Tri.maximum(t123)

    Tri.maximum(Tri.map(t123)(l => l * l)) should be(maxt123 * maxt123)
  }

  "Squaring (1)" should "be (1)" in {
    Tri.map(Leef(1))(l => l * l) should be(Leef(1))
  }

  "Squaring ((1, 2), 3)" should "be ((1, 4), (9))" in {
    val t123 = Brench(Brench(Leef(1), Leef(2)), Leef(3))
    val t149 = Brench(Brench(Leef(1), Leef(4)), Leef(9))

    Tri.map(t123)(l => l * l) should be(t149)
  }
}

class Exercise3_29 extends AnyFlatSpec with Matchers {
  "Finding tri maximum via folding" should "yield the same as `maximum`" in {
    val v = Random.nextInt()
    val leef = Leef(v)
    val t123 = Brench(Brench(Leef(1), Leef(2)), Leef(3))
    val tl1234 = Brench(Brench(Brench(Leef(1), Leef(2)), Leef(3)), Leef(4))
    val tr1234 = Brench(Brench(Leef(1), Leef(2)), Brench(Leef(3), Leef(4)))

    Tri.maximum(tr1234) should be(Tri.maximumViaFold(tr1234))
    Tri.maximum(tl1234) should be(Tri.maximumViaFold(tl1234))
    Tri.maximum(t123) should be(Tri.maximumViaFold(t123))
    Tri.maximum(leef) should be(Tri.maximumViaFold(leef))
  }

  "Finding tri size via folding" should "yield the same as `size`" in {
    val leef = Leef(1)
    val t2 = Brench(Leef(1), Leef(1))
    val tr4 = Brench(Leef(1), Brench(Leef(1), Brench(Leef(1), Leef(1))))
    val tl4 = Brench(Brench(Leef(1), Brench(Leef(1), Leef(1))), Leef(1))

    Tri.size(leef) should be(Tri.sizeViaFold(leef))
    Tri.size(t2) should be(Tri.sizeViaFold(t2))
    Tri.size(tl4) should be(Tri.sizeViaFold(tl4))
    Tri.size(tr4) should be(Tri.sizeViaFold(tr4))
  }

  "Finding tri depth via folding" should "yield the same as `depth`" in {
    val t12345 = Brench(Brench(Leef(1), Brench(Leef(2), Leef(3))), Brench(Leef(4), Leef(5)))
    val tb = Brench(Leef(1), Leef(1))
    val leef = Leef(1)

    Tri.depth(t12345) should be(Tri.depthViaFold(t12345))
    Tri.depth1(t12345) should be(Tri.depthViaFold(t12345))
    Tri.depth(leef) should be(Tri.depthViaFold(leef))
    Tri.depth1(leef) should be(Tri.depthViaFold(leef))
    Tri.depth(tb) should be(Tri.depthViaFold(tb))
    Tri.depth1(tb) should be(Tri.depthViaFold(tb))
  }

  "Squaring a tri via folding" should "yield the same as via `map`" in {
    val leef = Leef(1)
    val t123 = Brench(Brench(Leef(1), Leef(2)), Leef(3))

    def square(a: Int): Int = a * a

    Tri.map(leef)(l => l * l) should be(Tri.mapViaFold(leef)(square))
    Tri.map(t123)(l => l * l) should be(Tri.mapViaFold(t123)(square))
  }
}
