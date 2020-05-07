package fpinscala
package chapter3
package lizt

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

  def append[A](a1: Lizt[A], a2: Lizt[A]): Lizt[A] = a1 match {
    case Cons(head, tail) => Cons(head, append(tail, a2))
    case Nill             => a2
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
  // Excercise 3.13 (b)
  def foldRightViaFoldLeft[A, B](as: Lizt[A], z: B)(f: (A, B) => B): B =
    foldLeft(reverse(as), z)((a, b) => f(b, a))

  // Excercise 3.14
  def append2[A](as: Lizt[A], bs: Lizt[A]): Lizt[A] = foldRight(bs, as)(Cons(_, _))

  // Excercise 3.15
  def concat[A](lol: Lizt[Lizt[A]]): Lizt[A] = foldRightViaFoldLeft(lol, Nill: Lizt[A])(append)

  // Excercise 3.16
  def plus1(is: Lizt[Int]): Lizt[Int] =
    foldRightViaFoldLeft(is, Nill: Lizt[Int])((i, acc) => Cons(i + 1, acc))

  // Excercise 3.17
  def stringifyDoubles(ds: Lizt[Double]): Lizt[String] =
    foldRightViaFoldLeft(ds, Nill: Lizt[String])((d, acc) => Cons(d.toString, acc))

  // Excercise 3.18
  def map[A, B](as: Lizt[A])(f: A => B): Lizt[B] =
    foldRightViaFoldLeft(as, Nill: Lizt[B])((h, acc) => Cons(f(h), acc))

  // Excercise 3.19
  def filter[A](as: Lizt[A])(f: A => Boolean): Lizt[A] =
    foldRightViaFoldLeft(as, Nill: Lizt[A])((h, acc) => if (f(h)) Cons(h, acc) else acc)

  // Excercise 3.20
  def flatMap[A, B](as: Lizt[A])(f: A => Lizt[B]): Lizt[B] =
    foldRightViaFoldLeft(as, Nill: Lizt[B])((h, acc) => append(f(h), acc))

  def flatMapViaConcatAndMap[A, B](as: Lizt[A])(f: A => Lizt[B]): Lizt[B] = concat(map(as)(f))

  // Excercise 3.21
  def filterViaFlatMap[A](as: Lizt[A])(f: A => Boolean): Lizt[A] =
    flatMap(as)(a => if (f(a)) Lizt(a) else Nill)

  // Excercise 3.22
  def intLiztAdder(as: Lizt[Int], bs: Lizt[Int]): Lizt[Int] = (as, bs) match {
    case (_, Nill)                    => Nill
    case (Nill, _)                    => Nill
    case (Cons(ah, at), Cons(bh, bt)) => Cons(ah + bh, intLiztAdder(at, bt))
  }

  // Excercise 3.23
  def zipWith[A](as: Lizt[A], bs: Lizt[A])(f: (A, A) => A): Lizt[A] = (as, bs) match {
    case (_, Nill)                    => Nill
    case (Nill, _)                    => Nill
    case (Cons(ah, at), Cons(bh, bt)) => Cons(f(ah, bh), zipWith(at, bt)(f))
  }

  // Excercise 3.24
  @annotation.tailrec
  def startsWith[A](sup: Lizt[A], sub: Lizt[A]): Boolean = (sup, sub) match {
    case (_, Nill) => true
    case (Nill, _) => false
    case (Cons(h1, t1), Cons(h2, t2)) =>
      if (h1 == h2) startsWith(t1, t2)
      else false
  }

  @annotation.tailrec
  def hasSubsequence[A](sup: Lizt[A], sub: Lizt[A]): Boolean = sup match {
    case Nill => sub == Nill
    case Cons(head, tail) =>
      if (startsWith(sup, sub)) true
      else hasSubsequence(tail, sub)
  }
}
