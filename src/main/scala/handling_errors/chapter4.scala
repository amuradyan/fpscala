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
}
