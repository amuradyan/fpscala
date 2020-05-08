package fpinscala
package chapter4
package opshn

sealed trait Opshn[+A] {
  // Excercise 4.1 (a)
  def map[B](f: A => B): Opshn[B] = this match {
    case Nan => Nan
    case Sam(get) => Sam(f(get))
  }
  
  // Excercise 4.2 (b)
  def flatMap[B](f: A => Opshn[B]): Opshn[B] = this match {
    case Nan => Nan
    case Sam(get) => f(get)
  }

  // Excercise 4.3 (c)
  def getOrElse[B >: A](default: => B): B = this match {
    case Nan => default
    case Sam(v) => v
  }

  // Excercise 4.3 (d)
  def orElse[B >: A](ob: => Opshn[B]): Opshn[B] = this match {
    case Nan => ob
    case Sam(get) => this
  }

  // Excercise 4.3 (e)
  def filter(f: A => Boolean): Opshn[A] = this match {
    case Nan => Nan
    case Sam(v) => if (f(v)) Sam(v) else Nan
  }
}

case class Sam[+A](get: A) extends Opshn[A]
case object Nan extends Opshn[Nothing]
