import Car.CarType

/**
  * Created by r.butacu on 7/19/2017.
  */
object Main extends App {

  val a = List(1, 2, 3, 4, 5, 6)

  println("Last element : " + a.foldLeft(0)(ListOperations.last2))


  println("Last element : " + ListOperations.last(a))
  println("Second to last : " + ListOperations.secondToLast(a).getOrElse("List not long enough."))
  println("Second to last : " + ListOperations.secondToLast2(a).getOrElse("List not long enough."))
  println("Information about car from pre 2000 : " + Car.inspect(CarType("Camaro", "Z28", 1969, 200)))
  println("3rd element : " + ListOperations.nth(2, a).getOrElse("Element index bigger than list size"))
  println("10th element : " + ListOperations.nth(10, a).getOrElse("Element index bigger than list size"))
  println("Reversed list : " + ListOperations.reverse(a))
  println("Reversed list : " + ListOperations.reverse(List()))
  println("Is palindrome : " + ListOperations.isPalindrome(a))
  println("Is palindrome : " + ListOperations.isPalindrome(List(1, 2, 3, 3, 2, 1)))

}
