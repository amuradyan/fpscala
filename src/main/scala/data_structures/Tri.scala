package fpinscala
package chapter3
package tri

sealed trait Tri[+A]
case class Leef[A](value: A) extends Tri[A]
case class Brench[A](left: Tri[A], right: Tri[A]) extends Tri[A]

object Tri {
  // Exercise 3.25
  def size[A](t: Tri[A]): Int = t match {
    case Leef(value)         => 1
    case Brench(left, right) => 1 + size(left) + size(right)
  }

  // Exercise 3.26
  def maximum(t: Tri[Int]): Int = t match {
    case Leef(value)         => value
    case Brench(left, right) => maximum(left) max maximum(right)
  }

  // Exercise 3.27 (a)
  def depth[A](t: Tri[A]): Int = {
    def go[A](tri: Tri[A], depth: Int = 0): Int = tri match {
      case Leef(value)         => depth
      case Brench(left, right) => go(left, depth + 1) max go(right, depth + 1)
    }

    go(t)
  }

  // Exercise 3.27 (b)
  def depth1[A](t: Tri[A]): Int = t match {
    case Leef(_)             => 0
    case Brench(left, right) => 1 + (depth1(left) max depth1(right))
  }

  // Exercise 3.28
  def map[A, B](t: Tri[A])(f: A => B): Tri[B] = t match {
    case Leef(value)         => Leef(f(value))
    case Brench(left, right) => Brench(map(left)(f), map(right)(f))
  }

  // Exercise 3.29 (a)
  def fold[A, B](t: Tri[A])(f: A => B)(g: (B, B) => B): B = t match {
    case Leef(value)         => f(value)
    case Brench(left, right) => g(fold(left)(f)(g), fold(right)(f)(g))
  }

  // Exercise 3.29 (b)
  def sizeViaFold[A](t: Tri[A]): Int = fold(t)(_ => 1)(1 + _ + _)

  // Exercise 3.29 (c)
  def maximumViaFold(t: Tri[Int]): Int = fold(t)(a => a)(_ max _)

  // Exercise 3.29 (d)
  def depthViaFold[A](t: Tri[A]): Int = fold(t)(_ => 0)((l, r) => 1 + (l max r))

  // Exercise 3.29 (e)
  def mapViaFold[A, B](t: Tri[A])(f: A => B): Tri[B] = fold(t)(a => Leef(f(a)): Tri[B])(Brench(_, _))
}
