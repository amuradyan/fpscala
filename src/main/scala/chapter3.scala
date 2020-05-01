package fpinscala
package chapter3

object Chapter3 {

  sealed trait Lizt[+A]
  case object Nill extends Lizt[Nothing]
  case class Cons[A](head: A, tail: Lizt[A]) extends Lizt[A]

  object Lizt {
    def sum(ints: Lizt[Int]): Int = ints match {
      case Nill        => 0
      case Cons(x, xs) => x + sum(xs)
    }

    def product(doubles: Lizt[Double]): Double = doubles match {
      case Nill         => 1.0
      case Cons(0.0, _) => 0.0
      case Cons(x, xs)  => x * product(xs)
    }

    def apply[A](as: A*): Lizt[A] = {
      if (as.isEmpty) Nill
      else Cons(as.head, apply(as.tail: _*))
    }

    // Excercise 3.2
    def tail[A](as: Lizt[A]): Lizt[A] = as match {
      case Cons(head, tail) => tail
      case Nill             => Nill
    }

    // Excercise 3.3
    def setHead[A](a: A, as: Lizt[A]): Lizt[A] = as match {
      case Cons(head, tail) => Cons(a, tail)
      case Nill             => Cons(a, Nill)
    }

    // Excercise 3.4
    def drop[A](as: Lizt[A], n: Int): Lizt[A] = as match {
      case Cons(head, tail) => if (n > 0) Lizt.drop(tail, n - 1) else as
      case Nill             => Nill
  }
  }

  // Excercise 3.5
}
