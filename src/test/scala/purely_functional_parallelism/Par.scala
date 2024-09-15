package fpinscala
package chapter7
package tests

import org.scalatest.matchers.should._
import par._
import org.scalatest.freespec.AnyFreeSpec
import java.util.concurrent.Executors
import fpinscala.chapter3.lizt.Lizt

class ParTests extends AnyFreeSpec with Matchers {
  private val es = Executors.newFixedThreadPool(5)

  "Par should" - {
    "provide a convenient interface for" - {
      "summing integers" in {
        Par.sum(Lizt(1, 2, 3, 4)).run(es).get() shouldBe 10
      }

      "sorting lizts" in {
        Par.sortPar(Par.unit(Lizt(3, 4, 1, 2))).run(es).get shouldBe Lizt(1, 2, 3, 4)
      }
    }

    "be able to" - {
      "promote a constant value to a parallel computation" in {
        Par.unit(1).run(es).get should be(1)
      }

      "map a given function over Par" in {
        Par.unit(3).map(_ + 48).run(es).get shouldBe 51
      }

      "combine the results of two parallel computations" in {
        Par.unit(11).map2(Par.unit(2))(_ * _).run(es).get should be(22)
      }

      "mark a computation for concurrent evaluation" in {
        pending
      }

      "lazily mark a computation for concurrent evaluation" in {
        pending
      }

      "actually perform a computation and provide its' value" in {
        pending
      }

      "wrap any function into a lazy blanket" in {
        pending
      }
    }
  }
}
