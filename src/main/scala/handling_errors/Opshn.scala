package fpinscala
package chapter4
package opshn

sealed trait Opshn[+A] {
  // Excercise 4.1 (a)
  def map[B](f: A => B): Opshn[B] = this match {
    case Non => Non
    case Saam(get) => Saam(f(get))
  }
  
  // Excercise 4.2 (b)
  def flatMap[B](f: A => Opshn[B]): Opshn[B] = this match {
    case Non => Non
    case Saam(get) => f(get)
  }

  // Excercise 4.3 (c)
  def getOrElse[B >: A](default: => B): B = this match {
    case Non => default
    case Saam(v) => v
  }

  // Excercise 4.3 (d)
  def orElse[B >: A](ob: => Opshn[B]): Opshn[B] = this match {
    case Non => ob
    case Saam(get) => this
  }

  // Excercise 4.3 (e)
  def filter(f: A => Boolean): Opshn[A] = this match {
    case Non => Non
    case Saam(v) => if (f(v)) Saam(v) else Non
  }
}

case class Saam[+A](get: A) extends Opshn[A]
case object Non extends Opshn[Nothing]
