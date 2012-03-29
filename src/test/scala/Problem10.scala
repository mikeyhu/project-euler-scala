import org.scalatest.matchers.ShouldMatchers
import org.scalatest.FunSuite
import scala.annotation.tailrec
import utils.Primes

object Problem10 {
  def sumOfPrimes(under:Int) : Long = {
    new utils.Primes().takeWhile(_<under).foldLeft(0l)((sum:Long,num:Int) => sum+num)
  }
}



class Problem10tests extends FunSuite with ShouldMatchers {
  test("The sum of primes below 10 should be 17") {
    Problem10.sumOfPrimes(10) should be(17l)
  }
  
  test("The sum of primes below 2000000 should be 142913828922") {
    Problem10.sumOfPrimes(2000000) should be(142913828922l)
  }
}