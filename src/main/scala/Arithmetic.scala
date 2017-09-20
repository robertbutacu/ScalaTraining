import scala.math.sqrt
import Stream._

case class InvalidInputException(msg: String)

/**
  * Created by Robert-PC on 9/19/2017.
  */
object Arithmetic {
  def isPrime1(x: Int): Boolean = {
    x match {
      case _ if x < 2      => false
      case 2               => true
      case _ if x % 2 == 0 => false
      case _               =>
        !(3 to sqrt(x.toDouble).floor.toInt by 2).toArray.exists(x % _ == 0)
    }
  }

  def isPrime(x: Int): Boolean = {
    (x > 2) && (primes takeWhile {
      _ <= Math.sqrt(x.toInt)
    } forall (x % _ != 0))
  }

  lazy val primes: Stream[Int] = Stream.cons(2, Stream.from(3, 2).filter { x: Int => isPrime(x) })

  def listPrimesInRange(range: Range): List[Int] = {
    primes dropWhile { _ <= range.start } takeWhile { _ <= range.end} toList
  }

  def gcd(x: Int, y: Int): Int = {
    if (y == 0)
      x
    else
      gcd(y, x % y)
  }

  def printGoldbachList(range: Range) {
    range.toList.filter(_ % 2 == 0)
      .foreach(
        even =>
          goldbach(even) match {
            case Left((x, y)) => println(even + " = " + x + " + " + y)
            case _            =>
          }
      )
  }

  def goldbach(x: Int): Either[(Int, Int), InvalidInputException] = {
    primes takeWhile { _ < x } find { p => isPrime(x - p)} match {
      case None     => Right(InvalidInputException("Not found!"))
      case Some(p1) => Left((p1, x - p1))
    }
  }

  def isCoprime(x: Int, y: Int): Boolean = {
    if(gcd(x, y) == 1)
      true
    else
      false
  }

  def totient(m: Int): Int = (1 to m).count(isCoprime(m, _))


  /*
    List(1,2,3,4,5,6)
    List(2,3,4,5,6)

    zipping the previous lists would give the result
    List((1,2),(2,3),(3,4),(4,5),(5,6))

    Being a stream, it goes like this:
    0 and 1 are already found, followed by
    (0,1).zip(1) => (0,1) => 1
    0, 1, 1
    (0, 1, 1).zip((1, 1)) => (0,1), (1,1) => 1, 2
    and so on.
   */
  lazy val fibs: Stream[BigInt] = BigInt(0) #::
    BigInt(1) #::
    fibs.zip(fibs.tail).map { n => n._1 + n._2 }
}