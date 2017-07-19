/**
  * Created by r.butacu on 7/19/2017.
  */
object Main extends App {

  val a = List(1, 2, 3, 4, 5, 6)

  println("Last element : " + a.foldLeft(0)(new ListOperations().last2))


  println("Last element : " + new ListOperations().last(a))
  println("Second to last : " + new ListOperations().secondToLast(a).getOrElse("List not long enough."))
  println("Second to last : " + new ListOperations().secondToLast2(a).getOrElse("List not long enough."))
}
