package fpinscala
package chapter3
import fpinscala.chapter3.Chapter3.Nill
import fpinscala.chapter3.Chapter3.Cons

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

    def append[A](a1: Lizt[A], a2: Lizt[A]): Lizt[A] = {
      a1 match {
        case Cons(head, tail) => Cons(head, append(tail, a2))
        case Nill             => a2
      }
    }

    def foldRight[A, B](as: Lizt[A], z: B)(f: (A, B) => B): B = as match {
      case Cons(head, tail) => f(head, foldRight(tail, z)(f))
      case Nill             => z
    }

    def sum2(as: Lizt[Int]) = foldRight(as, 0)((a, b) => a + b)

    def product2(as: Lizt[Double]) = foldRight(as, 1.0)(_ * _)

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
      case Cons(head, tail) => if (n > 0) drop(tail, n - 1) else as
      case Nill             => Nill
    }

    // Excercise 3.5
    def dropWhile[A](as: Lizt[A])(f: A => Boolean): Lizt[A] = as match {
      case Cons(head, tail) =>
        if (f(head)) dropWhile(tail)(f)
        else Cons(head, dropWhile(tail)(f))
      case Nill => Nill
    }

    // Excercise 3.6
    def init[A](as: Lizt[A]): Lizt[A] = as match {
      case Cons(last, Nill) => Nill
      case Cons(head, tail) => Cons(head, init(tail))
      case Nill             => Nill
    }

    // Excercise 3.9
    def length[A](as: Lizt[A]): Int = foldRight(as, 0)((a, b) => b + 1)

    // Excercise 3.10
    @annotation.tailrec
    def foldLeft[A, B](as: Lizt[A], z: B)(f: (B, A) => B): B = as match {
      case Cons(head, tail) => foldLeft(tail, f(z, head))(f)
      case Nill             => z
    }

    // Excercise 3.11 (a)
    def sumFoldl(as: Lizt[Int]) = foldLeft(as, 0)(_ + _)
    // Excercise 3.11 (b)
    def productFoldl(as: Lizt[Double]) = foldLeft(as, 1.0)(_ * _)
    // Excercise 3.11 (c)
    def lengthFoldl[A](as: Lizt[A]) = foldLeft(as, 0)((acc, _) => acc + 1)

    // Excercise 3.12
    def reverse[A](as: Lizt[A]) = foldLeft(as, Nill: Lizt[A])((l, e) => Cons(e, l))

    // Excercise 3.13 (a)
    def foldLeftViaFoldRight[A, B](as: Lizt[A], z: B)(f: (B, A) => B): B =
      foldRight(reverse(as), z)((a, b) => f(b, a))
    // Excercise 3.13 (b.1)
    def foldRightViaFoldLeft[A, B](as: Lizt[A], z: B)(f: (A, B) => B): B =
      foldLeft(reverse(as), z)((a, b) => f(b, a))

    // Excercise 3.14
    def append2[A](as: Lizt[A], bs: Lizt[A]): Lizt[A] = foldRight(bs, as)(Cons(_, _))
  }
}
