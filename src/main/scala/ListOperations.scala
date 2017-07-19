/**
  * Created by r.butacu on 7/19/2017.
  */
object ListOperations {

  def last(a: List[Int]): Int = {
    a match {
      case h :: Nil => h
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



}
