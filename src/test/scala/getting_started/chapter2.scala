package fpinscala
package chapter2
package tests

import org.scalatest.matchers.should._
import org.scalatest.flatspec.AnyFlatSpec
import scala.util.Random

class Exercise2_1 extends AnyFlatSpec with Matchers {
  "A Fibonacci calculator" should "return 0 for negative indicies" in {
    val idx = Random.nextInt(100)
    Chapter2.Fib(-idx) should be (0);
  }
  
  it should "return 0 for 0th index" in {
    Chapter2.Fib(0) should be (0);
  }

  it should "return 1 for 1st index" in {
    Chapter2.Fib(1) should be (1);
  }

  it should "return 1 for 2nd index" in {
    Chapter2.Fib(2) should be (1);
  }

  it should "return 2 for 3rd index" in {
    Chapter2.Fib(3) should be (2);
  }

  it should "return 3 for 4th index" in {
    Chapter2.Fib(4) should be (3);
  }

  it should "return 55 for 10th index" in {
    Chapter2.Fib(10) should be (55)
  }
}

class Exercise2_2 extends AnyFlatSpec with Matchers {
  val emptyArray = Array.empty[Int]
  val singleElementArray = Array(1)
  val ascArray = Array(1, 2, 3, 4)
  val descArray = Array(4, 3, 2, 1)
  val sqrArray = Array(2, 4, 16, 256)
  val ascStringArray = Array("a", "b", "c", "d")

  def asc(a: Int, b: Int) = a < b
  def ascString(a: String, b: String) = a < b
  def desc(a: Int, b: Int) = a > b
  def sqr(a: Int, b: Int) = b == (a * a)

  "An ordering check function" should "return true for empty arrays with any ordering function" in {
    Chapter2.isSorted(emptyArray, asc) should be (true)
    Chapter2.isSorted(emptyArray, desc) should be (true)
  }

  it should "return true for single element arrays with any ordering function" in {
    Chapter2.isSorted(singleElementArray, asc) should be (true)
    Chapter2.isSorted(singleElementArray, desc) should be (true)
  }

  it should "return true for ASC sorted array and ASC order function" in {
    Chapter2.isSorted(ascArray, asc) should be (true)
  }

  it should "return true for DESC sorted array and DESC order function" in {
    Chapter2.isSorted(descArray, desc) should be (true)
  }

  it should "return true for SQR sorted array and SQR order function" in {
    Chapter2.isSorted(sqrArray, sqr) should be (true)
  }

  it should "return true for ASC sorted array and ASC order function for strings as well" in {
    Chapter2.isSorted(ascStringArray, ascString) should be (true)
  }
}