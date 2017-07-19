/**
  * Created by r.butacu on 7/19/2017.
  */
object Main extends App {

  val a = List(1, 1, 2, 3, 5, 8)

  println(a.foldLeft(0)(new ListOperations().last))

  println(new ListOperations().getLast(a))
  println(new ListOperations().getSecondToLast(a))
}
