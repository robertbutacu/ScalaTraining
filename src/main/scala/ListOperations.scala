/**
  * Created by r.butacu on 7/19/2017.
  */
class ListOperations {
  def last(a : Int, b : Int) : Int = {
    b
  }

  def getLast( a : List[Int]) : Int ={
    a.tail.isEmpty match {
      case true => a.head
      case false => getLast(a.tail)
    }
  }

}
