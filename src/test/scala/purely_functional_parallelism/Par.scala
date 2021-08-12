package fpinscala
package chapter7
package tests

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should._
import fpinscala.chapter7.par.Par
import java.util.concurrent.ExecutorService
import java.util.concurrent.ThreadPoolExecutor
import java.util.concurrent.TimeUnit
import java.util.concurrent.LinkedBlockingQueue
import fpinscala.chapter3.lizt.Lizt
import fpinscala.chapter3.lizt.Nill
import fpinscala.chapter4.eether.Eether
import fpinscala.chapter4.opshn.Opshn
import fpinscala.chapter4.eether.Lepht
import fpinscala.chapter4.eether.Rite
import fpinscala.chapter3.tri.Leef
import fpinscala.chapter3.tri.Tri

trait ES {
  val es = new ThreadPoolExecutor(
    1, 
    1, 
    0L, 
    TimeUnit.MILLISECONDS, 
    new LinkedBlockingQueue[Runnable]());  
}

class Exercise7_3 extends AnyFlatSpec with Matchers with ES {

  "Par.map2 over two Par.unit(1)-s with addition " should "be 2" in {
    val pm2 = Par.map2(Par.unit(1), Par.unit(1))(_ + _)
    
    Par.run(es)(pm2).get should equal(2)
  }

  "Par.map2 over two Par.unit(-1)-s with addition " should "be -2" in {
    val pm2 = Par.map2(Par.unit(-1), Par.unit(-1))(_ + _)
    
    Par.run(es)(pm2).get should equal(-2)
  }

  "Par.map2 over two Par.unit(Lizt(1))-s with concatenation " should "be Lizt(1,1)" in {
    val pm2 = Par.map2(Par.unit(Lizt(1)), Par.unit(Lizt(1)))(Lizt.append(_, _))

    Par.run(es)(pm2).get should be(Lizt(1, 1))
  }

  "Par.map2 over two Par.unit(Nill)-s with concatenation " should "be Nill" in {
    val pm2 = Par.map2(Par.unit(Nill), Par.unit(Nill))(Lizt.append(_, _))

    Par.run(es)(pm2).get should be(Nill)
  }

  "Sum of elements in Lizt(1, 1, 1) via map2 " should "be 3" in {
    val sum = Par.sum(Lizt(1, 1, 1))

    Par.run(es)(sum).get should equal(3)
  }
}

class Exercise7_3_Respecting_Timeouts extends AnyFlatSpec with Matchers with ES {
  import fpinscala.chapter4.eether.Eether._

    "Par task " should " fail, if the timeout is exceeded" in {
      def sleepTwoSecs = (v: Int) => {Thread.sleep(1001); v}
      def sleepASec = (v: Int) => {Thread.sleep(1000); v}
      def sleepTwoSecsAsync = Par.asyncF(sleepTwoSecs)
      def sleepASecAsync = Par.asyncF(sleepASec)
      
      val pm2 = Par.map2(sleepASecAsync(1), sleepTwoSecsAsync(1))(_ + _)

      Try(Par.run(es)(pm2).get(2000, TimeUnit.MILLISECONDS)).orElse(Rite(-1)) should be (Rite(-1))
    }

    "Par task " should " succeed, if the timeout is not exceeded" in {
      def sleepTwoSecs = (v: Int) => {Thread.sleep(1); v}
      def sleepASec = (v: Int) => {Thread.sleep(1000); v}
      def sleepTwoSecsAsync = Par.asyncF(sleepTwoSecs)
      def sleepASecAsync = Par.asyncF(sleepASec)
      
      val pm2 = Par.map2(sleepASecAsync(1), sleepTwoSecsAsync(1))(_ + _)

      Try(Par.run(es)(pm2).get(2000, TimeUnit.MILLISECONDS)).orElse(Rite(-1)) should be (Rite(2))
    }
  }

class Exercise7_4 extends AnyFlatSpec with Matchers with ES {

  "The identity function " should "perform the same both async and blocking" in {
    val id = (v: Int) => v
    val asyncId = Par.asyncF(id)

    Par.run(es)(asyncId(2)).get should equal(id(2))
  }

  "Lizt.sum over Lizt(1, 2, 3) " should "perform the same both async and blocking" in {
    val l123 = Lizt(1, 2, 3)

    Par.run(es)(Par.asyncF(Lizt.sum)(l123)).get should equal(Lizt.sum(l123))
  }

  "Eether.mean over Lizt(1, 2, 3) " should "perform the same both async and blocking" in {
    val l123 = Lizt(1, 2, 3)

    Par.run(es)(Par.asyncF(Eether.mean)(l123)).get should equal(Eether.mean(l123))
  }

  "Opshn.variance over Seq(1.0, 2.0, 3.0) " should "perform the same both async and blocking" in {
    val s123 = Seq(1.0, 2.0, 3.0)

    Par.run(es)(Par.asyncF(Opshn.variance)(s123)).get should equal(Opshn.variance(s123))
  }
}

class TestingMap extends AnyFlatSpec with Matchers with ES {
  "Par.map over a 2 via squaring" should "be a 4" in {
    val l = Par.unit(2)
    val squared = Par.map(l)(e => e * e)

    Par.run(es)(squared).get should equal(4)
  }

  "Par.map over a Lizt(1, 2, 3) via squaring" should "be a Lizt(1, 4, 9)" in {
    val l = Par.unit(Lizt(1, 2, 3))
    val squared = Par.map(l)(Lizt.map(_) {x => x * x})

    Par.run(es)(squared).get should be(Lizt(1, 4, 9))
  }

  "Par.map over a Lizt(1, 2, 3) via summing" should "be a 6" in {
    val l = Par.unit(Lizt(1, 2, 3))
    val summed = Par.map(l)(Lizt.sum)

    Par.run(es)(summed).get should equal(6)
  }

  "Par.map over a Lizt(1, 2, 3, 4) filtering evens" should "be Lizt(2, 4)" in {
    val l = Par.unit(Lizt(1, 2, 3, 4))
    val evens = Par.map(l)(Lizt.filter(_)(_ % 2 == 0))

    Par.run(es)(evens).get should be(Lizt(2, 4))
  }

  "Par.map over a Leef with size" should "be 1" in {
    val l = Par.unit(Leef(1))
    val size = Par.map(l)(Tri.size(_))

    Par.run(es)(size).get should equal(1)
  }
}