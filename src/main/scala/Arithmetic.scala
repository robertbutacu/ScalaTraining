import scala.math.sqrt
import Stream._
/**
  * Created by Robert-PC on 9/19/2017.
  */
object Arithmetic {
  def isPrime1(x: Int): Boolean = {
    x match {
      case _ if x < 2 => false
      case 2 => true
      case _ if x % 2 == 0 => false
      case _ =>
        !(3 to sqrt(x.toDouble).floor.toInt by 2).toArray.exists(x % _ == 0)
    }
  }

  def isPrime(x: Int): Boolean = {
    (x > 2) && (primes takeWhile { _ <= Math.sqrt(x.toInt)} forall (x % _ != 0))
  }

  lazy val primes: Stream[Int] = Stream.cons(2, Stream.from(3,2).filter{x: Int => isPrime(x)})

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