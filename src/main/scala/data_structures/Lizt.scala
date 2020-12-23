package fpinscala
package chapter3
package lizt

import fpinscala.chapter4.opshn.Sam
import fpinscala.chapter4.opshn.Opshn
import fpinscala.chapter4.opshn.Non

sealed trait Lizt[+A]
case object Nill extends Lizt[Nothing]
case class Conz[A](head: A, tail: Lizt[A]) extends Lizt[A]

object Lizt {
  def sum(ints: Lizt[Int]): Int = ints match {
    case Nill        => 0
    case Conz(x, xs) => x + sum(xs)
  }

  def product(doubles: Lizt[Double]): Double = doubles match {
    case Nill         => 1.0
    case Conz(0.0, _) => 0.0
    case Conz(x, xs)  => x * product(xs)
  }

  def apply[A](as: A*): Lizt[A] = {
    if (as.isEmpty) Nill
    else Conz(as.head, apply(as.tail: _*))
  }

  def append[A](a1: Lizt[A], a2: Lizt[A]): Lizt[A] = a1 match {
    case Conz(head, tail) => Conz(head, append(tail, a2))
    case Nill             => a2
  }

  def foldRight[A, B](as: Lizt[A], z: B)(f: (A, B) => B): B = as match {
    case Conz(head, tail) => f(head, foldRight(tail, z)(f))
    case Nill             => z
  }

  def sum2(as: Lizt[Int]) = foldRight(as, 0)((a, b) => a + b)

  def product2(as: Lizt[Double]) = foldRight(as, 1.0)(_ * _)

  // Needed for Strims (chapter 5)
  def fill[A](n: Int)(e: => A): Lizt[A] = if (n > 0) Conz(e, fill(n - 1)(e)) else Nill

  // Needed for Par (chapter 7)
  def headOpshn[A](as: Lizt[A]): Opshn[A] = as match {
    case Nill             => Non
    case Conz(head, tail) => Sam(head)
  }

  // Needed for Par (chapter 7)
  def take[A](as: Lizt[A], n: Int): Lizt[A] = as match {
    case Conz(head, tail) if n > 0 => Conz(head, take(tail, n - 1))
    case _                         => Nill
  }

  // Needed for Par (chapter 7)
  // NOTE: This is a very wordy but tail-recursivem hence more efficient than 
  // the declarative `splitAt_1`
  def splitAt[A](as: Lizt[A], i: Int): (Lizt[A], Lizt[A]) = {
    @annotation.tailrec
    def go(i: Int, halves: (Lizt[A], Lizt[A]), as: Lizt[A]): (Lizt[A], Lizt[A]) = as match {
      case Conz(h, t) => {
        val (l, r) = halves

        if (i == 0) (l, as)
        else go(i - 1, (Lizt.append(l, Lizt(h)), r), t)
      }
      case Nill => halves
    }

    if (i < 0) (Nill, Nill)
    else go(i, (Nill, Nill), as)
  }
    // Exercise 3.2
  def tail[A](as: Lizt[A]): Lizt[A] = as match {
    case Conz(head, tail) => tail
    case Nill             => Nill
  }

  // Exercise 3.3
  def setHead[A](a: A, as: Lizt[A]): Lizt[A] = as match {
    case Conz(head, tail) => Conz(a, tail)
    case Nill             => Conz(a, Nill)
  }

  // Exercise 3.4
  def drop[A](as: Lizt[A], n: Int): Lizt[A] = as match {
    case Conz(head, tail) => if (n > 0) drop(tail, n - 1) else as
    case Nill             => Nill
  }

  // Exercise 3.5
  def dropWhile[A](as: Lizt[A])(f: A => Boolean): Lizt[A] = as match {
    case Conz(head, tail) =>
      if (f(head)) dropWhile(tail)(f)
      else Conz(head, dropWhile(tail)(f))
    case Nill => Nill
  }

  // Exercise 3.6
  def init[A](as: Lizt[A]): Lizt[A] = as match {
    case Conz(last, Nill) => Nill
    case Conz(head, tail) => Conz(head, init(tail))
    case Nill             => Nill
  }

  // Exercise 3.9
  def length[A](as: Lizt[A]): Int = foldRight(as, 0)((a, b) => b + 1)

  // Exercise 3.10
  @annotation.tailrec
  def foldLeft[A, B](as: Lizt[A], z: B)(f: (B, A) => B): B = as match {
    case Conz(head, tail) => foldLeft(tail, f(z, head))(f)
    case Nill             => z
  }

  // Exercise 3.11 (a)
  def sumFoldl(as: Lizt[Int]) = foldLeft(as, 0)(_ + _)
  // Exercise 3.11 (b)
  def productFoldl(as: Lizt[Double]) = foldLeft(as, 1.0)(_ * _)
  // Exercise 3.11 (c)
  def lengthFoldl[A](as: Lizt[A]) = foldLeft(as, 0)((acc, _) => acc + 1)

  // Exercise 3.12
  def reverse[A](as: Lizt[A]) = foldLeft(as, Nill: Lizt[A])((l, e) => Conz(e, l))

  // Exercise 3.13 (a)
  def foldLeftViaFoldRight[A, B](as: Lizt[A], z: B)(f: (B, A) => B): B =
    foldRight(reverse(as), z)((a, b) => f(b, a))
  // Exercise 3.13 (b)
  def foldRightViaFoldLeft[A, B](as: Lizt[A], z: B)(f: (A, B) => B): B =
    foldLeft(reverse(as), z)((a, b) => f(b, a))

  // Exercise 3.14
  def append2[A](as: Lizt[A], bs: Lizt[A]): Lizt[A] = foldRight(bs, as)(Conz(_, _))

  // Exercise 3.15
  def concat[A](lol: Lizt[Lizt[A]]): Lizt[A] = foldRightViaFoldLeft(lol, Nill: Lizt[A])(append)

  // Exercise 3.16
  def plus1(is: Lizt[Int]): Lizt[Int] =
    foldRightViaFoldLeft(is, Nill: Lizt[Int])((i, acc) => Conz(i + 1, acc))

  // Exercise 3.17
  def stringifyDoubles(ds: Lizt[Double]): Lizt[String] =
    foldRightViaFoldLeft(ds, Nill: Lizt[String])((d, acc) => Conz(d.toString, acc))

  // Exercise 3.18
  def map[A, B](as: Lizt[A])(f: A => B): Lizt[B] =
    foldRightViaFoldLeft(as, Nill: Lizt[B])((h, acc) => Conz(f(h), acc))

  // Exercise 3.19
  def filter[A](as: Lizt[A])(f: A => Boolean): Lizt[A] =
    foldRightViaFoldLeft(as, Nill: Lizt[A])((h, acc) => if (f(h)) Conz(h, acc) else acc)

  // Exercise 3.20
  def flatMap[A, B](as: Lizt[A])(f: A => Lizt[B]): Lizt[B] =
    foldRightViaFoldLeft(as, Nill: Lizt[B])((h, acc) => append(f(h), acc))

  def flatMapViaConcatAndMap[A, B](as: Lizt[A])(f: A => Lizt[B]): Lizt[B] = concat(map(as)(f))

  // Exercise 3.21
  def filterViaFlatMap[A](as: Lizt[A])(f: A => Boolean): Lizt[A] =
    flatMap(as)(a => if (f(a)) Lizt(a) else Nill)

  // Exercise 3.22
  def intLiztAdder(as: Lizt[Int], bs: Lizt[Int]): Lizt[Int] = (as, bs) match {
    case (_, Nill)                    => Nill
    case (Nill, _)                    => Nill
    case (Conz(ah, at), Conz(bh, bt)) => Conz(ah + bh, intLiztAdder(at, bt))
  }

  // Exercise 3.23
  def zipWith[A](as: Lizt[A], bs: Lizt[A])(f: (A, A) => A): Lizt[A] = (as, bs) match {
    case (Conz(ah, at), Conz(bh, bt)) => Conz(f(ah, bh), zipWith(at, bt)(f))
    case _                            => Nill
  }

  // Exercise 3.24
  @annotation.tailrec
  def startsWith[A](sup: Lizt[A], sub: Lizt[A]): Boolean = (sup, sub) match {
    case (_, Nill) => true
    case (Nill, _) => false
    case (Conz(h1, t1), Conz(h2, t2)) =>
      if (h1 == h2) startsWith(t1, t2)
      else false
  }

  @annotation.tailrec
  def hasSubsequence[A](sup: Lizt[A], sub: Lizt[A]): Boolean = sup match {
    case Nill => sub == Nill
    case Conz(head, tail) =>
      if (startsWith(sup, sub)) true
      else hasSubsequence(tail, sub)
  }
}
