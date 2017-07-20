import org.scalatest.FlatSpec

/**
  * Created by r.butacu on 7/20/2017.
  */
class ListOperationsSpec extends FlatSpec {
  "A list" should "be reversed after calling reverse function" in {
    assert(ListOperations.reverse(List(1, 2, 3)) === List(3, 2, 1))
    assert(ListOperations.reverse(List(1)) === List(1))
  }
  it should "return empty list when calling on empty list" in {
    assert(ListOperations.reverse(Nil) === List())
  }
  it should "return empty list when calling on empty list(badReverse)" in {
    assert(ListOperations.badReverse(Nil) === List())
  }
  it should "be reversed after calling badReverse function" in {
    assert(ListOperations.badReverse(List(1, 2, 3, 4, 5, 6, 7)) === List(7, 6, 5, 4, 3, 2, 1))
    assert(ListOperations.badReverse(List(1)) === List(1))
  }
  it should "return true after calling isPalindrome function on palindrome list" in {
    assert(ListOperations.isPalindrome(List(1, 2, 3, 3, 2, 1)) === true)
    assert(ListOperations.isPalindrome(List(1)) === true)
    assert(ListOperations.isPalindrome(List()) === true)
  }
  it should "return false after calling isPalindrome on non-palindrome list " in {
    assert(ListOperations.isPalindrome(List(1, 2, 3)) === false)

  }
  it should "return nth element after nth function is called" in {
    assert(ListOperations.nth(2, List(1, 2, 3)).get === 3)
    assert(ListOperations.nth(0, List(1, 2, 3)).get === 1)
  }
  it should "return none on invalid input (nth function)" in {
    assert(ListOperations.nth(3, List(1, 2)) === None)
    assert(ListOperations.nth(5, Nil) === None)
  }
  it should "return last element after last function is called(last function)" in {
    assert(ListOperations.last(List(1, 2, 3)).get === 3)
    assert(ListOperations.last(List(1)).get === 1)
  }
  it should "return none on empty lists(last function)" in {
    assert(ListOperations.last(Nil) === None)
  }
  it should "return second to last (secondToLast)" in {
    assert(ListOperations.secondToLast(List(1, 2, 3)).get === 2)
  }
  it should "return none on secondToLast function" in {
    assert(ListOperations.secondToLast(List(1)) === None)
    assert(ListOperations.secondToLast(List()) === None)
  }
  it should "return second to last (secondToLast2)" in {
    assert(ListOperations.secondToLast2(List(1, 2, 3)).get === 2)
  }
  it should "return none on secondToLast2 function" in {
    assert(ListOperations.secondToLast2(List(1)) === None)
    assert(ListOperations.secondToLast2(List()) === None)
  }
  it should "return last element(last function)" in {
    assert(ListOperations.last(List(1, 2, 3)).get === 3)
    assert(ListOperations.last(List(1)).get === 1)
  }
  it should "return none(last function)" in {
    assert(ListOperations.last(Nil) === None)
  }
  it should "flatten list" in {
    assert(ListOperations.flatten(List(List(1, 2, 3), 4, 3, List(123))) === List(1, 2, 3, 4, 3, 123))
    assert(ListOperations.flatten(List(1, 2, 3, 4, 5, 6)) === List(1, 2, 3, 4, 5, 6))
  }
  it should "remove duplicates" in {
    assert(ListOperations.compress(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) === List('a, 'b, 'c, 'a, 'd, 'e))
    assert(ListOperations.compress(List('a, 'b, 'c, 'd)) === List('a, 'b, 'c, 'd))
  }
  it should "pack duplicates " in {
    assert(ListOperations.pack(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) === List(List('a, 'a, 'a, 'a), List('b), List('c, 'c), List('a, 'a), List('d), List('e, 'e, 'e, 'e)))
    assert(ListOperations.pack(List()) === List())
    assert(ListOperations.pack(List('a)) === List(List('a)))
    assert(ListOperations.pack(List('a, 'b, 'c)) === List(List('a), List('b), List('c)))
  }
  it should "encode lists" in {
    assert(ListOperations.encode(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) === List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e)))
  }

}
