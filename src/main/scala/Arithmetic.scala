import scala.math.sqrt
/**
  * Created by Robert-PC on 9/19/2017.
  */
object Arithmetic {
  def isPrime(x: Int): Boolean = {
    x match {
      case _ if x < 2      => false
      case 2               => true
      case _ if x % 2 == 0 => false
      case _               =>
        !(3 to sqrt(x.toDouble).floor.toInt by 2).toArray.exists(x % _ == 0)
    }
  }
}
