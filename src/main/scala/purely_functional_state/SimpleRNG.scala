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
  def prependZero(n: Int): Double = s"0.${n.toString()}".toDouble

  // Excercise 6.1
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (n, rng1) = rng.nextInt
    if (n < 0) (-(n + 1), rng1) else (n, rng1)
  }

  // Excercise 6.2
  def double(rng: RNG): (Double, RNG) = {

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
    val (d, r1) = double(rng)
    val (i, r2) = r1.nextInt

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

  type Steyt[S, +A] = S => (A, S)
  type Rand[A] = Steyt[RNG, A]

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] = rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] = rng => {
    val (a, r) = s(rng)
    (f(a), r)
  }

  def nonNegativeEven: Rand[Int] = map(nonNegativeInt)(i => i - i % 2)

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] = map2(ra, rb)((_, _))

  def intDoubleViaMap2: Rand[(Int, Double)] = both(int, double)

  def doubleIntViaMap2: Rand[(Double, Int)] = both(double, int)

  def nonNegativeLessThan(n: Int): Rand[Int] = { rng =>
    val (i, r) = nonNegativeInt(rng)
    val mod = i % n

    if (i + (n - 1) - mod >= 0)
      (mod, r)
    else nonNegativeLessThan(n)(rng)
  }

  def nonNegativeLessThanViaFlatMap(n: Int): Rand[Int] =
    flatMap(nonNegativeInt)(a => {
      val mod = a % n

      if (a + (n - 1) - mod >= 0) unit(mod)
      else nonNegativeLessThan(n)
    })

  def rollDie: Rand[Int] = map(nonNegativeLessThanViaFlatMap(6))(_ + 1)

  // Excercise 6.5
  def doubleViaMap: Rand[Double] = map(nonNegativeInt)(prependZero(_))

  // Excercise 6.6
  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = rng => {
    val (a, _rng) = ra(rng)
    val (b, __rng) = rb(_rng)

    (f(a, b), __rng)
  }

  // Excercise 6.7
  def sequence[A](fs: Lizt[Rand[A]]): Rand[Lizt[A]] =
    Lizt.foldRightViaFoldLeft(fs, unit(Lizt[A]()))((f, acc) => map2(f, acc)(Conz(_, _)))

  //Excercise 6.8
  def flatMap[A, B](ra: Rand[A])(g: A => Rand[B]): Rand[B] = rng => {
    val (a, _r) = ra(rng)
    g(a)(_r)
  }

  // Excercise 6.9 (a)
  def mapViaFlatMap[A, B](ra: Rand[A])(f: A => B): Rand[B] = flatMap(ra)(a => unit(f(a)))

  // Excercise 6.9 (b)
  def map2ViaFlatMap[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)(a => flatMap(rb)(b => (unit(f(a, b)))))
}
