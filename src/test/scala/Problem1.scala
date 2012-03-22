import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

object Problem1 {
  def sumOfNumbers(size : Int) = {
    (1 until size).foldLeft[Int](0)((sum,next) => next match {
      case num if(num % 3 == 0 || num % 5 == 0) => sum + next
      case _ => sum
    })
  }
}

class Problem1Tests extends FunSuite with ShouldMatchers{

  test("Sum of Natural numbers below 10 that are multiples of 3 or 5") {
    Problem1.sumOfNumbers(10) should be(23)
  }
  
  test("Sum of Natural numbers below 1000 that are multiples of 3 or 5") {
    Problem1.sumOfNumbers(1000) should be(233168)
  }
}