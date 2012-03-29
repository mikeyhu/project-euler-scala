package utils
import scala.annotation.tailrec

class Primes(thisPrime:Int=2, previousPrimes : Vector[Int] = Vector(2)) extends Stream[Int] {
  private val next = nextPrime(thisPrime+1)
  
  override def head = thisPrime
  
  override def tail = {
    new Primes(next, previousPrimes:+next)
  }
  protected def tailDefined = -1!=next
  override def isEmpty = !tailDefined
  
  @tailrec
  private def nextPrime(num:Int) : Int = {
    if(num==Int.MaxValue) -1
    else if(isPrime(num,previousPrimes)) num
    else nextPrime(num+1)
  }
  
  @tailrec
  private def isPrime(numberToCheck:Int, primes :Vector[Int]) : Boolean = {
    if(primes.isEmpty || primes.head > math.sqrt(numberToCheck)) true
    else if(numberToCheck % primes.head == 0) false
    else isPrime(numberToCheck, primes.tail)
  }
}