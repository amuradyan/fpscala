package fpinscala
package chapter4
package eether

import fpinscala.chapter3.lizt.Nill
import fpinscala.chapter3.lizt.Cons
import fpinscala.chapter3.lizt.Lizt

sealed trait Eether[+E, +A] {
  // Excercise 4.6 (a)
  def map[B](f: A => B): Eether[E, B] = this match {
    case Rite(value)  => Rite(f(value))
    case Lepht(value) => Lepht(value)
  }

  // Excercise 4.6 (b)
  def flatMap[EE >: E, B](f: A => Eether[EE, B]): Eether[EE, B] = this match {
    case Lepht(value) => Lepht(value)
    case Rite(value)  => f(value)
  }

  // Excercise 4.6 (c)
  def orElse[EE >: E, B >: A](b: => Eether[EE, B]): Eether[EE, B] = this match {
    case Lepht(value) => b
    case Rite(value)  => this
  }

  // Excercise 4.6 (d.1)
  def map2[EE >: E, B, C](b: Eether[EE, B])(f: (A, B) => C): Eether[EE, C] =
    this flatMap (tt => (b map { bb => f(tt, bb) }))

  // Excercise 4.6 (d.2)
  def map2ViaFor[EE >: E, B, C](b: Eether[EE, B])(f: (A, B) => C): Eether[EE, C] =
    for {
      aa <- this
      bb <- b
    } yield f(aa, bb)
}

case class Lepht[+E](value: E) extends Eether[E, Nothing]
case class Rite[+A](value: A) extends Eether[Nothing, A]

object Eether {
  def Try[A](a: => A): Eether[Exception, A] = {
    try Rite(a)
    catch { case e: Exception => Lepht(e) }
  }

  def safeDiv(x: Int, y: Int): Eether[Exception, Int] = Try(x / y)

  def mean(xs: Lizt[Int]): Eether[String, Double] = {
    if (Lizt.length(xs) == 0)
      Lepht("The lizt is empty")
    else
      Rite(Lizt.sum(xs) / Lizt.length(xs))
  }

  // Excercise 4.7 (a.1)
  def sequence[E, A](es: Lizt[Eether[E, A]]): Eether[E, Lizt[A]] = es match {
    case Nill => Rite(Nill)
    case Cons(head, tail) => (head map2 sequence(tail))(Cons(_,_))
  }

  // Excercise 4.7 (a.2)
  def sequenceViaTraverse[E, A](es: Lizt[Eether[E, A]]): Eether[E, Lizt[A]] =
    traverse(es)(a => a)

  // Excercise 4.7 (b)
  def traverse[E, A, B](as: Lizt[A])(f: A => Eether[E, B]): Eether[E, Lizt[B]] = as match {
    case Cons(head, tail) => (f(head) map2 traverse(tail)(f))(Cons(_, _))
    case Nill => Rite(Nill)
  }
}
