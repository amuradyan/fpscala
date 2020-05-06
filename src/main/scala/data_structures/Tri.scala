package fpinscala
package chapter3
package tri

sealed trait Tri[+A]
case class Leef[A](value: A) extends Tri[A]
case class Brench[A](left: Tri[A], right: Tri[A]) extends Tri[A]

object Tri {
  // Excercise 3.25
  def size[A](t: Tri[A]): Int = t match {
    case Leef(value)         => 1
    case Brench(left, right) => 1 + size(left) + size(right)
  }

  // Excercise 3.26
  def maximum(t: Tri[Int]): Int = t match {
    case Leef(value)         => value
    case Brench(left, right) => maximum(left) max maximum(right)
  }

  // Excercise 3.27
  def depth[A](t: Tri[A]): Int = {
    def go[A](tri: Tri[A], depth: Int = 0): Int = tri match {
      case Leef(value)         => depth + 1
      case Brench(left, right) => go(left, depth + 1) max go(right, depth + 1)
    }

    go(t)
  }

  // Excercise 3.28
  def map[A, B](tri: Tri[A])(f: A => B): Tri[B] = tri match {
    case Brench(left, right) => Brench(map(left)(f), map(right)(f))
    case Leef(value) => Leef(f(value))
  }
}
