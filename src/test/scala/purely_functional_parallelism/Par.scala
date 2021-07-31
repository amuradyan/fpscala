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

class Exercise7_1 extends AnyFlatSpec with Matchers {
  val es = new ThreadPoolExecutor(
    1, 
    1, 
    0L, 
    TimeUnit.MILLISECONDS, 
    new LinkedBlockingQueue[Runnable]());

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