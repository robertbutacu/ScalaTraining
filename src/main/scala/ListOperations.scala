import scala.annotation.tailrec
/**
  * Created by r.butacu on 7/19/2017.
  */
object ListOperations {

  @tailrec
  def last(a: List[Int]): Option[Int] = {
    a match {
      case Nil => None
      case h :: Nil => Some(h)
      case h :: t => last(t)
    }
  }

  def last2(a: Int, b: Int): Int = {
    a
  }

  def flatten[AnyVal](input: List[AnyVal]): List[AnyVal] = {
    @tailrec
    def go[AnyVal](input: List[AnyVal], output: List[AnyVal]): List[AnyVal] = {
      input match {
        case Nil => output
        case (h: List[AnyVal]) :: (tail: List[AnyVal]) => go(tail, output ::: flatten(h))
        case (h: AnyVal) :: (tail: List[AnyVal]) => go(tail, output.:+(h))
      }
    }

    go(input, Nil)
  }

  //TODO tailrec
  def compress(input: List[Symbol]): List[Symbol] = {
    input match {
      case Nil => Nil
      case t :: Nil => List(t)
      case (h: Symbol) :: (t: Symbol) :: (tail: List[Symbol]) if h == t => compress(t :: tail)
      case h :: tail => h :: compress(tail)
    }
  }

  def pack(input: List[Any]): List[List[Symbol]] = {
    input match {
      //empty list
      case Nil => List()
      //List(List(a,a,...),a,something)
      case (t: List[Symbol]) :: (h: Symbol) :: tail if t.head == h => pack(t.:+(h) :: tail)
      //List(List(a,a,...),b,something)
      case (t: List[Symbol]) :: (h: Symbol) :: tail if t.head != h => t :: pack(h :: tail)
      //List(List(a,a,...),Nil)
      case (t: List[Symbol]) :: Nil => List(t)
      //List(a,a,something)
      case (t: Symbol) :: (h: Symbol) :: tail if t == h => pack(List(t, h) :: tail)
      //List(a,something)
      case (t: Symbol) :: tail => List(t) :: pack(tail)
    }
  }

  def encode(input: List[Symbol]): List[Tuple2[Int, Symbol]] = {
    @tailrec
    def go(input: List[List[Symbol]], result: List[Tuple2[Int, Symbol]]): List[Tuple2[Int, Symbol]] = {
      input match {
        case Nil => result
        case (t: List[Symbol]) :: tail => go(tail, result.:+((t.size, t.head)))
      }
    }

    go(pack(input), Nil)
  }

  def decode(input : List[Tuple2[Int, Symbol]]) : List[Symbol] = {
    @tailrec
    def go(input : List[Tuple2[Int, Symbol]], result : List[Symbol]) : List[Symbol] = {
      input match {
        case Nil => result
        case (count, symbol) :: tail if count == 0 => go(tail, result)
        case (count, symbol) :: tail if count > 0 => go((count - 1, symbol) :: tail, result.:+(symbol))
      }
    }
    go(input, Nil)
  }


  @tailrec
  def secondToLast(a: List[Int]): Option[Int] = {
    a.size match {
      case invalid if invalid < 2 => None
      case 2 => Some(a.head)
      case _ => secondToLast(a.tail)
    }
  }

  @tailrec
  def secondToLast2(a: List[Int]): Option[Int] = {
    a match {
      case Nil | _ :: Nil => None
      case h1 :: h2 :: Nil => Some(h1)
      case h1 :: h2 :: tail => secondToLast2(h2 :: tail)
    }
  }

  @tailrec
  def nth(index: Int, a: List[Int]): Option[Int] = {
    if (a.isEmpty)
      None
    else
      index match {
        case 0 => Some(a.head)
        case _ => nth(index - 1, a.tail)
      }
  }


  def reverse(a: List[Int]): List[Int] = {
    @tailrec
    def go(input: List[Int], accum: List[Int]): List[Int] = {
      input match {
        case Nil => accum
        case _ => go(input.tail, accum.+:(input.head))
      }
    }

    go(a, Nil)
  }

  def badReverse(a: List[Int]): List[Int] = {
    a match {
      case Nil => Nil
      case _ => reverse(a.tail) ++ List(a.head)
    }
  }

  def isPalindrome(a: List[Int]): Boolean = a.equals(this.reverse(a))

}
