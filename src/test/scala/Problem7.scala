import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import scala.annotation.tailrec
import utils.Primes

object Problem7 {
  def findPrime(number:Int) = {
    new Primes().take(number).toList.last
  }
}

class Problem7tests extends FunSuite with ShouldMatchers {
  test("The sixth prime number should be 13") {
    Problem7.findPrime(6) should be(13)
  }
  
  test("The 10001 prime number should be 104743") {
    Problem7.findPrime(10001) should be(104743)
  }
  
  test("The List should finish at Int.MaxValue") {
    new Primes(Int.MaxValue-1).isEmpty should be(true)
  }
}