package fpinscala
package chapter5
package strim

import Strim._
import scala.annotation.tailrec
import scala.collection.mutable
import fpinscala.chapter3.lizt.Lizt
import fpinscala.chapter3.lizt.Nill
import fpinscala.chapter3.lizt
import fpinscala.chapter4.opshn.Sam
import fpinscala.chapter4.opshn.Non
import fpinscala.chapter4.opshn.Opshn

sealed trait Strim[+A] {
  def headOpshn: Opshn[A] = this match {
    case Conz(h, t) => Sam(h())
    case Emptie     => Non
  }

  def exists(p: A => Boolean): Boolean = this match {
    case Conz(h, t) => p(h()) || t().exists(p)
    case Emptie     => false
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Conz(h, t) => f(h(), t().foldRight(z)(f))
    case Emptie     => z
  }

  def existsViaFoldRight(p: A => Boolean): Boolean =
    foldRight(false)(p(_) || _)

  def find(p: A => Boolean): Opshn[A] = filter(p).headOpshn

  // Excercise 5.1 (1)
  def toLizt: Lizt[A] = this match {
    case Conz(h, t) => lizt.Conz(h(), t().toLizt)
    case Emptie     => Nill
  }

  // Excercise 5.1 (2)
  def toLiztRec: Lizt[A] = {
    // Since the mutable buffer does not escape our method
    // this is still _pure_ fp
    // Also this implementation is stack-safe
    val buf = mutable.ListBuffer[A]()

    @annotation.tailrec
    def go(s: Strim[A]): List[A] =
      s match {
        case Conz(h, t) => {
          buf += h()
          go(t())
        }
        case Emptie => buf.toList
      }

    Lizt(go(this): _*) // Have to explode to use Lizts
  }

  // Excercise 5.2 (a)
  def take(n: Int): Strim[A] = this match {
    case Conz(h, t) => if (n > 0) Conz(h, () => t().take(n - 1)) else Emptie
    case Emptie     => Emptie
  }

  // Excercise 5.2 (b)
  def drop(n: Int): Strim[A] = this match {
    case Conz(h, t) => if (n > 0) t().drop(n - 1) else this
    case Emptie     => Emptie
  }

  // Excercise 5.3
  def takeWhile(p: A => Boolean): Strim[A] = this match {
    case Conz(h, t) if p(h()) => Conz(h, () => t().takeWhile(p))
    case _                    => Emptie
  }

  // Excercise 5.4
  def forAll(p: A => Boolean): Boolean = foldRight(true)(p(_) && _)

  // Excercise 5.5
  def takeWhileViaFoldRight(p: A => Boolean): Strim[A] =
    foldRight(Emptie: Strim[A])((h, acc) =>
      if (p(h)) Conz(() => h, () => acc)
      else acc
    )

  // Excercise 5.6
  def headOpshnViaFoldRight: Opshn[A] =
    foldRight(Non: Opshn[A])((c, _) => Sam(c))

  // Excercise 5.7 (a)
  def map[B](f: A => B): Strim[B] =
    foldRight(emptie[B])((c, r) => Conz(() => f(c), () => r))

  // Excercise 5.7 (b)
  def flatMap[B](f: A => Strim[B]): Strim[B] =
    foldRight(emptie[B])(f(_) append _)

  // Excercise 5.7 (c)
  def filter(f: A => Boolean): Strim[A] =
    foldRight(Emptie: Strim[A])((c, r) =>
      if (f(c)) Conz(() => c, () => r)
      else r
    )

  // Excercise 5.7 (d)
  def append[B >: A](bs: => Strim[B]): Strim[B] =
    foldRight(bs)((a, b) => Conz(() => a, () => b))

  // Excercise 5.13 (a)
  def mapViaUnfold[B](f: A => B): Strim[B] = unfold(this) {
    case Conz(h, t) => Sam((f(h()), t()))
    case Emptie     => Non
  }

  // Excercise 5.13 (b)
  def takeViaUnfold(n: Int): Strim[A] = unfold((this, n)) {
    case (_, 0)          => Non
    case (Emptie, _)     => Non
    case (Conz(h, t), c) => Sam(h(), (t(), c - 1))
  }

  // Excercise 5.13 (c)
  def takeWhileViaUnfold(p: A => Boolean): Strim[A] = unfold(this) {
    case Conz(h, t) if (p(h())) => Sam((h(), t()))
    case _                      => Non
  }

  // Excercise 5.13 (d)
  def zipWith[B >: A](as: Strim[B])(f: (A, B) => B): Strim[B] = unfold((this, as)) {
    case (Conz(h1, t1), Conz(h2, t2)) => Sam((f(h1(), h2()), (t1(), t2())))
    case _                            => Non
  }

  // Excercise 5.13 (e)
  def zipAll[B](bs: Strim[B]): Strim[(Opshn[A], Opshn[B])] = unfold((this, bs)) {
    case (Conz(h1, t1), Conz(h2, t2)) => Sam((Sam(h1()), Sam(h2())), (t1(), t2()))
    case (Conz(h1, t1), Emptie)       => Sam(((Sam(h1()), Non), (t1(), emptie[B])))
    case (Emptie, Conz(h2, t2))       => Sam(((Non, Sam(h2())), (emptie[A], t2())))
    case _                            => Non
  }

  // Excercise 5.14
  def startsWith[A](as: Strim[A]): Boolean = (this, as) match {
    case (_, Emptie) => false
    case (Emptie, _) => false
    case _ =>
      zipAll(as) takeWhile { !_._2.isEmpty } forAll {
        case (h, h2) => h == h2
      }
  }
}

case class Conz[+A](h: () => A, t: () => Strim[A]) extends Strim[A]
case object Emptie extends Strim[Nothing]

object Strim {
  def conz[A](hd: => A, tl: => Strim[A]): Strim[A] = {
    lazy val h = hd
    lazy val t = tl

    Conz(() => h, () => t)
  }

  def emptie[A]: Strim[A] = Emptie

  def apply[A](as: A*): Strim[A] =
    if (as.isEmpty) emptie else conz(as.head, apply(as.tail: _*))

  val ones: Strim[Int] = conz(1, ones)

  // Excercise 5.8 (a)
  def constant[A](a: A): Strim[A] = Conz(() => a, () => constant(a))

  // Excercise 5.8 (b)
  def constantLazy[A](a: A): Strim[A] = {
    lazy val tail: Strim[A] = Conz(() => a, () => tail)
    tail
  }

  // Excercise 5.9
  def from(n: Int): Strim[Int] = Conz(() => n, () => from(n + 1))

  // Excercise 5.10
  def fibs: Strim[Int] = {
    def fib(a: Int, b: Int): Strim[Int] = {
      Conz(() => a, () => fib(b, a + b))
    }

    fib(0, 1)
  }

  // Excercise 5.11
  def unfold[A, S](z: S)(f: S => Opshn[(A, S)]): Strim[A] = f(z) match {
    case Sam((h, z)) => conz(h, unfold(z)(f))
    case Non         => emptie
  }

  // Excercise 5.12 (a)
  def fibsViaUnfold: Strim[Int] = unfold((0, 1)) {
    case (a, b) => Sam(a, (b, a + b))
  }

  // Excercise 5.12 (b)
  def fromViaUnfold(n: Int): Strim[Int] = unfold(n) { v => Sam(v, v + 1) }

  // Excercise 5.12 (a)
  def constantViaUnfold(c: Int): Strim[Int] = unfold(c) { v => Sam(v, v) }

  // Excercise 5.12 (a)
  def onesViaUnfold: Strim[Int] = unfold(1) { v => Sam(v, v) }
}
