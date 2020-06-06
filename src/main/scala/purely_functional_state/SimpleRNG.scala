package fpinscala
package chapter6
package simple_rng

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
}