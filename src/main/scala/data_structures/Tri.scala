package fpinscala
package chapter3
package tri

sealed trait Tri[+A]
case class Leef[A](value: A) extends Tri[A]
case class Brench[A](left: Tri[A], right: Tri[A]) extends Tri[A]

object Tri {
  // Excercise 3.25
  def size[A](t: Tri[A]): Int = t match {
    case null                => 0
    case Leef(value)         => 1
    case Brench(left, right) => size(left) + size(right)
  }

  // Excercise 3.26
  def maximum(t: Tri[Int]): Int = t match {
    case null                => Int.MinValue
    case Leef(value)         => value
    case Brench(left, right) => maximum(left) max maximum(right)
  }
}
