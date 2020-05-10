package fpinscala
package chapter4
package opshn

import chapter3.lizt._

sealed trait Opshn[+A] {
  // Excercise 4.1 (a)
  def map[B](f: A => B): Opshn[B] = this match {
    case Non => Non
    case Sam(get) => Sam(f(get))
  }
  
  // Excercise 4.1 (b)
  def flatMap[B](f: A => Opshn[B]): Opshn[B] = this match {
    case Non => Non
    case Sam(get) => f(get)
  }

  // Excercise 4.1 (c)
  def getOrElse[B >: A](default: => B): B = this match {
    case Non => default
    case Sam(v) => v
  }

  // Excercise 4.1 (d)
  def orElse[B >: A](ob: => Opshn[B]): Opshn[B] = this match {
    case Non => ob
    case Sam(get) => this
  }

  // Excercise 4.1 (e)
  def filter(f: A => Boolean): Opshn[A] = this match {
    case Non => Non
    case Sam(v) => if (f(v)) Sam(v) else Non
  }
}

case class Sam[+A](get: A) extends Opshn[A]
case object Non extends Opshn[Nothing]

object Excercises {
  // Excercise 4.2
  def variance(xs: Seq[Double]): Opshn[Double] = {

    def mean(xs: Seq[Double]): Opshn[Double] =
      if (xs.isEmpty) Non
      else Sam(xs.sum / xs.length)

    mean(xs) flatMap (m => mean(xs.map(v => math.pow(v - m, 2))))
  }

  // Excercise 4.3
  def map2[A, B, C](a: Opshn[A], b: Opshn[B])(f: (A, B) => C): Opshn[C] =
    a flatMap (aa => (b map (bb => f(aa, bb))))

  def map2_2[A, B, C](a: Opshn[A], b: Opshn[B])(f: (A, B) => C): Opshn[C] = (a, b) match {
    case (_, Non)         => Non
    case (Non, _)         => Non
    case (Sam(a), Sam(b)) => Sam(f(a, b))
  }

  def insuranceRateQuote(age: Int, numberOfSpeedingTickets: Int): Double = ???

  def lift[A, B](f: A => B): Opshn[A] => Opshn[B] = _ map f

  def Try[A](a: => A): Opshn[A] = {
    try Sam(a)
    catch { case e: Exception => Non }
  }

  def parseInsuranceRateQuote(age: String, numberOfSpeedingTickets: String): Opshn[Double] = {
    val optAge: Opshn[Int] = Try(age.toInt)
    val optNumberOfSpeedingTickets: Opshn[Int] = Try(numberOfSpeedingTickets.toInt)

    map2(optAge, optNumberOfSpeedingTickets)(insuranceRateQuote)
  }

  // Excercise 4.4 (a)
  def sequence[A](xs: Lizt[Opshn[A]]): Opshn[Lizt[A]] = xs match {
    case Cons(head, tail) => head flatMap (h => sequence(tail) map (Cons(h, _)))
    case Nill => Sam(Nill)
  }

  // Excercise 4.4 (b)
  def sequenceViaFoldRight[A](xs: Lizt[Opshn[A]]): Opshn[Lizt[A]] = 
    Lizt.foldRightViaFoldLeft[Opshn[A], Opshn[Lizt[A]]](xs, Sam(Nill))(map2(_, _)(Cons(_, _)))

  // Excercise 4.4 (c)
  def sequenceViaTraverse[A](as: Lizt[Opshn[A]]) = traverse(as)(a => a)

  // Excercise 4.5 (a)
  def traverse[A, B](as: Lizt[A])(f: A => Opshn[B]): Opshn[Lizt[B]] = as match {
    case Nill => Sam(Nill)
    case Cons(h, t) => f(h) flatMap(hh => traverse(t)(f) map {l => Cons(hh, l)})
  }

  // Excercise 4.5 (b)
  def traverseViaFoldRight[A, B](as: Lizt[A])(f: A => Opshn[B]): Opshn[Lizt[B]] =
    Lizt.foldRightViaFoldLeft[A, Opshn[Lizt[B]]](as, Sam(Nill))((e, acc) => map2_2(f(e), acc)(Cons(_, _)))
}
