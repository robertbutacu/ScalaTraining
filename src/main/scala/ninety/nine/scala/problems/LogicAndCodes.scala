package ninety.nine.scala.problems

/**
  * Created by Robert-PC on 9/20/2017.
  */
object LogicAndCodes {

  def not(first: Boolean): Boolean = if(first) false else true

  def and(first: Boolean, second: Boolean): Boolean = {
    (first, second) match {
      case (true, true) => true
      case _            => false
    }
  }

  def or(first:Boolean, second: Boolean): Boolean = {
    (first, second) match {
      case (true, _)      => true
      case (_, true)      => true
      case (false, false) => false
    }
  }

  def nand(first: Boolean, second: Boolean): Boolean = not(and(first, second))

  def xor(first: Boolean, second: Boolean): Boolean = {
    (first, second) match {
      case (true, true)   => false
      case (false, false) => false
      case _              => true
    }
  }
}
