package fpinscala
package chapter4

import opshn._

object Excercises {
  // Excercise 4.2
  def variance(xs: Seq[Double]): Opshn[Double] = {

    def mean(xs: Seq[Double]): Opshn[Double] =
      if (xs.isEmpty) Non
      else Saam(xs.sum / xs.length)

    mean(xs) flatMap (m => mean(xs.map(v => math.pow(v - m, 2))))
  }

  // Excercise 4.3
  def map2[A, B, C](a: Opshn[A], b: Opshn[B])(f:(A, B) => C): Opshn[C] = 
    a flatMap (aa => (b map (bb => f(aa, bb))))

  def map2_2[A, B, C](a: Opshn[A], b: Opshn[B])(f:(A, B) => C): Opshn[C] = (a, b) match {
    case (_, Non) => Non
    case (Non, _) => Non
    case (Saam(a), Saam(b)) => Saam(f(a, b))
  }

  def insuranceRateQuote(age: Int, numberOfSpeedingTickets: Int): Double = ???

  def lift[A, B](f: A => B): Opshn[A] => Opshn[B] = _ map f

  def Try[A](a: => A): Opshn[A] = {
    try Saam(a)
    catch { case e: Exception => Non }
  }

  def parseInsuranceRateQuote(age: String, numberOfSpeedingTickets: String): Opshn[Double] = {
    val optAge: Opshn[Int] = Try(age.toInt)
    val optNumberOfSpeedingTickets: Opshn[Int] = Try(numberOfSpeedingTickets.toInt)

    map2(optAge, optNumberOfSpeedingTickets)(insuranceRateQuote)
  }
}
