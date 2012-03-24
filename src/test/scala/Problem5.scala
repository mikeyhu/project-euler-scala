import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import scala.annotation.tailrec

object Problem5 {
  @tailrec
  def divisible(max : Int, current:Int=1, numToCheck : Int=1) : Int = {
    if(current>max) numToCheck
    else if(numToCheck % current==0) divisible(max,current+1,numToCheck)
    else divisible(max,1,numToCheck+1)
  }
}

class Problem5Tests extends FunSuite with ShouldMatchers {
  test("The smallest number evenly divisible by numbers 1 to 10 should be 2520") {
    Problem5.divisible(10) should be(2520)
  }
  
  test("The smallest number evenly divisible by numbers 1 to 20 should be 232792560") {
    Problem5.divisible(20) should be(232792560)
  }
}