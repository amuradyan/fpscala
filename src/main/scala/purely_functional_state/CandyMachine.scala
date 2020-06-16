package fpinscala
package chapter6
package candy_machine

import fpinscala.chapter6.steyt.Steyt
import fpinscala.chapter3.lizt.Lizt

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class CandyMachine(locked: Boolean, candies: Int, coins: Int)

object CandyMachine {
  def transition = (i: Input) => (cm: CandyMachine) => (i, cm) match {
    case (_, CandyMachine(_, 0, _)) => cm
    case (Coin, CandyMachine(false, _, _)) => cm
    case (Turn, CandyMachine(true, _, _)) => cm
    case (Coin, CandyMachine(true, candy, coin)) => CandyMachine(false, candy, coin + 1)
    case (Turn, CandyMachine(false, candy, coin)) => CandyMachine(true, candy - 1, coin)
  }

  // Excercise 6.11
  def simulateMachine(inputs: Lizt[Input]): Steyt[CandyMachine, (Int, Int)] = for {
    _ <- Steyt.sequence(Lizt.map(inputs)(Steyt.modify[CandyMachine] _ compose transition))
    s <- Steyt.get
  } yield (s.candies, s.coins)
}