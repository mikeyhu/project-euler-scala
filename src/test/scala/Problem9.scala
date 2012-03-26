import org.scalatest.matchers.ShouldMatchers
import org.scalatest.FunSuite

object Problem9 {
  def triplet(total:Int) = {for { 
      b <- 2 to total/2
      a <- 1 to b-1
      val c=total-a-b
      if (a*a)+(b*b)==(c*c)
    } yield (a*b*c)}.head
}

class Problem9tests extends FunSuite with ShouldMatchers {
  test("The product of 3 numbers that sum to 12 that are a pythagorean triplet should be 60") {
    Problem9.triplet(12) should be(60) 
  }
  
  test("The product of 3 numbers that sum to 1000 that are a pythagorean triplet should be 31875000") {
    Problem9.triplet(1000) should be(31875000) 
  }
}