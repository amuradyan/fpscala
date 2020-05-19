package fpinscala
package chapter5
package strim

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
    case Conz(h, t) => if (p(h())) Conz(h, () => t().takeWhile(p)) else t().takeWhile(p)
    case Emptie     => Emptie
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
}
