package fpinscala
package chapter6
package tests

import fpinscala.chapter3.lizt.Lizt
import fpinscala.chapter6.candy_machine.Coin
import fpinscala.chapter6.candy_machine.Turn
import fpinscala.chapter6.candy_machine.CandyMachine
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Exercise6_11 extends AnyFlatSpec with Matchers {
  val emptyLockedMachine = CandyMachine(true, 0, 10)
  val emptyUnlockedMachine = CandyMachine(false, 0, 10)
  val fullLockedMachine = CandyMachine(true, 10, 10)
  val fullUnlockedMachine = CandyMachine(false, 10, 10)
  val twoCandies = CandyMachine(true, 2, 0)

  "Inserting a coin into a locked machine" should "cause it to unlock if there's any candy left" in {
    CandyMachine.simulateMachine(Lizt(Coin)).run(fullLockedMachine)._2 should be(CandyMachine(false, 10, 11))
  }

  "Turning the knob on an unlocked machine" should "cause it to dispense candy and become locked" in {
    CandyMachine.simulateMachine(Lizt(Turn)).run(fullUnlockedMachine)._2 should be(CandyMachine(true, 9, 10))
  }

  "Turning the knob on a locked machine" should "do nothing" in {
    CandyMachine.simulateMachine(Lizt(Turn)).run(emptyLockedMachine)._2 should be(emptyLockedMachine)
    CandyMachine.simulateMachine(Lizt(Turn)).run(fullLockedMachine)._2 should be(fullLockedMachine)
  }

  "Inserting a coin into an unlocked machine" should "do nothing" in {
    CandyMachine.simulateMachine(Lizt(Coin)).run(emptyUnlockedMachine)._2 should be(emptyUnlockedMachine)
    CandyMachine.simulateMachine(Lizt(Coin)).run(fullUnlockedMachine)._2 should be(fullUnlockedMachine)
  }

  "A machine that’s out of candy" should "ignore all inputs" in {
    CandyMachine.simulateMachine(Lizt(Coin)).run(emptyLockedMachine)._2 should be(emptyLockedMachine)
    CandyMachine.simulateMachine(Lizt(Coin)).run(emptyUnlockedMachine)._2 should be(emptyUnlockedMachine)
    CandyMachine.simulateMachine(Lizt(Turn)).run(emptyLockedMachine)._2 should be(emptyLockedMachine)
    CandyMachine.simulateMachine(Lizt(Turn)).run(emptyUnlockedMachine)._2 should be(emptyUnlockedMachine)
  }

  "Tossing two coins and taking two candies" should "exhaust the `twoCandies` machine" in {
    CandyMachine.simulateMachine(Lizt(Coin, Turn, Coin, Turn)).run(twoCandies)._2 should be(
      CandyMachine(true, 0, 2)
    )
  }
}
