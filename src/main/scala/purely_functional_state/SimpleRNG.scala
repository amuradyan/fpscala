package fpinscala
package chapter6
package simple_rng

import scala.annotation.tailrec
import fpinscala.chapter3.lizt.Nill
import fpinscala.chapter3.lizt.Conz
import fpinscala.chapter3.lizt.Lizt

trait RNG {
  def nextInt: (Int, RNG)
}

case class SimpleRNG(seed: Long) extends RNG {

  // Linear congruental generator
  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }
}

object RNG {
  // Excercise 6.1
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (n, rng1) = rng.nextInt
    if(n < 0) (-(n + 1), rng1) else (n, rng1)
  }

  // Excercise 6.2
  def double(rng: RNG): (Double, RNG) = {
    def prependZero(n: Int): Double = s"0.${n.toString()}".toDouble

    val (n, r) = nonNegativeInt(rng);
    (prependZero(n), r)
  }

  // Excercise 6.3 (a)
  def intDouble(rng: RNG): (Int, Double) = {
    val (i, r1) = rng.nextInt
    val (d, r2) = double(r1)

    (i, d)
  }

  // Excercise 6.3 (b)
  def doubleInt(rng: RNG): (Double, Int) = {
    val (i, r1) = rng.nextInt
    val (d, r2) = double(r1)

    (d, i)
  }

  // Excercise 6.3 (c)
  def double3(rng: RNG): (Double, Double, Double) = {
    val (d1, r1) = double(rng)
    val (d2, r2) = double(r1)
    val (d3, r3) = double(r2)

    (d1, d2, d3)
  }

  // Excercise 6.4
  def ints(count: Int)(rng: RNG): (Lizt[Int], RNG) = {
    @annotation.tailrec
    def loop(c: Int, s: (Lizt[Int], RNG)): (Lizt[Int], RNG) = {
      val (i, r) = s._2.nextInt
      if (c > 0) loop(c - 1, (Conz(i, s._1), r)) else s
    }

    loop(count, (Nill: Lizt[Int], rng))
  }
}