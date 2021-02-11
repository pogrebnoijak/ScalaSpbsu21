package org.spbsu.mkn.scala

import com.sun.tools.attach.VirtualMachine.list

import scala.::
import scala.collection.IterableOnce.iterableOnceExtensionMethods
import scala.io.StdIn.readLine
import scala.util.Random
import scala.util.Random.shuffle
import scala.util.control.Breaks.break

object TheGame {

  sealed trait GuessResult
  case class Correct(numTries: Int) extends GuessResult
  case class Incorrect(bulls: Int, cows: Int) extends GuessResult

  case class RepeatingDigitsException() extends RuntimeException
  case class WrongNumberLengthException(expected: Int, got: Int) extends RuntimeException

  def generateNumberString(length: Int): String = {
    val list = ('A' to 'Z').toList ++ ('a' to 'z').toList ++ ('0' until '9').toList
    shuffle(list).take(length).mkString("")
  }

  def validate(secret: String, userInput: String, numTries: Int = 1): GuessResult = {
    if (userInput.length != secret.length) throw WrongNumberLengthException(secret.length, userInput.length)
    val setUserInput = userInput.toSet
    if (setUserInput.size != userInput.length) throw RepeatingDigitsException()

    val bulls = (secret zip userInput).count(p => p._1 == p._2)
    if (bulls == secret.length) return Correct(numTries)
    val cowsAndBulls = secret.length - (setUserInput -- secret.toSet).size
    Incorrect(bulls, cowsAndBulls - bulls)
  }

  def main(args: Array[String]): Unit = {
    for (i <- args) {
      println(s"New game! Word have $i letters. Guess it.")
      val secret = generateNumberString(i.toInt)
      var tries = 1
      var win = false
      while(!win) {
        try {
          validate(secret, readLine(), tries) match {
            case Correct(num) => println(s"You guessed it on $num attempts!"); win = true
            case Incorrect(b, c) => println(s"bulls: $b, cows: $c"); tries += 1;
          }
        }
        catch {
          case _: RepeatingDigitsException => println("Letters should not be repeated!")
          case e: WrongNumberLengthException => println(s"Word length must be ${e.expected} and not ${e.got}")
        }
      }
    }
  }
}
