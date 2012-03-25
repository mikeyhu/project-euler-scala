import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import scala.annotation.tailrec

object Problem7 {
  def findPrime(number:Int) = {
    new Primes().take(number).toList.last
  }
}

class Primes(thisPrime:Int=2) extends Stream[Int] {
  private val next = nextPrime(thisPrime+1)
  
  override def head = thisPrime
  
  override def tail = {
    new Primes(next)
  }
  protected def tailDefined = -1!=next
  override def isEmpty = !tailDefined
  
  @tailrec
  private def nextPrime(num:Int) : Int = {
    if(num==Int.MaxValue) -1
    else if(isPrime(num)) num
    else nextPrime(num+1)
  }
  
  @tailrec
  private def isPrime(numberToCheck:Int, division:Int=2) : Boolean = {
    if(division > math.sqrt(numberToCheck)) true
    else if(numberToCheck % division == 0) false
    else isPrime(numberToCheck,division+1)
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