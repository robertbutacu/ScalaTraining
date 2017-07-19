/**
  * Created by r.butacu on 7/19/2017.
  */
object ListOperations {

  def last(a: List[Int]): Option[Int] = {
    a match {
      case Nil => None
      case h :: Nil => Some(h)
      case h :: t => last(t)
    }
  }

  def last2(a: Int, b: Int): Int = {
    b
  }

  def secondToLast(a: List[Int]): Option[Int] = {
    a.size match {
      case invalid if invalid < 2 => None
      case 2 => Some(a.head)
      case _ => secondToLast(a.tail)
    }
  }

  def secondToLast2(a: List[Int]): Option[Int] = {
    a match {
      case Nil | _ :: Nil => None
      case h1 :: h2 :: Nil => Some(h1)
      case h1 :: h2 :: tail => secondToLast2(tail)
    }
  }

  def nth(index: Int, a: List[Int]): Option[Int] = {
    if (a.isEmpty)
      None
    else
      index match {
        case 0 => Some(a.head)
        case _ => nth(index - 1, a.tail)
      }
  }

  //sth fishy here
  def reverse(a: List[Int]): List[Int] = {
    a match {
      case Nil => Nil
      case _ => reverse(a.tail) ++ List(a.head)
    }
  }

  def isPalindrome(a: List[Int]): Boolean = a.equals(this.reverse(a))

}
