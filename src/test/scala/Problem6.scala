import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

object Problem6 {
  def difference(max:Int) = {
    val sums = (1 to max).foldLeft((0,0))((sum,cur) => (sum._1+cur,sum._2+(cur*cur)))
    (sums._1*sums._1)-sums._2
  }
}

class Problem6tests extends FunSuite with ShouldMatchers {
  test("The difference between the sum of the squares and the square of the sum of numbers 1 to 10 should be 2640") {
    Problem6.difference(10) should be(2640)
  }
  
  test("The difference between the sum of the squares and the square of the sum of numbers 1 to 20 should be 25164150") {
    Problem6.difference(100) should be(25164150)
  }
}