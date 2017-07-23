import scala.concurrent.duration._
import scala.concurrent.{Await, Future, duration}
import scala.util.{Failure, Random, Success}
import scala.concurrent.ExecutionContext.Implicits.global

/**
  * Created by Robert-PC on 7/23/2017.
  */

object FutureExample extends App {
  type Computation = Int
  type Result = String

  def complicatedComputation() : Future[Int] = Future {
    println("[1] Starting complicated Computation")
    Thread.sleep(Random.nextInt(30000) + 10000)
    println("[1] Finished very complicated Computation!!")
    42
  }

  def anotherComplicatedComputation() : Future[Int] = Future {
    println("[2] Another complicated computation!")
    Thread.sleep(Random.nextInt(10000))
    println("[2] Finished another complicated Computation!")
    24
  }

  def composeResult(firstPart : Computation, secondPart : Computation) : Result = {
    println("[3]Composing the result!")
    Thread.sleep(Random.nextInt(10000))
    println("[3] Finished composing the result!")
    val result = firstPart + secondPart
    s"And the result is $firstPart + $secondPart = $result"
  }

  def compute(): Future[Result] ={
    val firstPart = complicatedComputation()
    val secondPart = anotherComplicatedComputation()
    for {
      firstPartComputed <- firstPart
      secondPartComputed <- secondPart
    } yield composeResult(firstPartComputed, secondPartComputed)
  }

  val startComputation = compute()
  Await.ready(startComputation, Duration(100, "minutes"))
  startComputation.onComplete({
    case Success(result) => {
      println(result)
    }
    case Failure(exception) => {
      println(exception)
    }
  })
}
