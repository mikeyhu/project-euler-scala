import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import scala.annotation.tailrec

object Problem3 {
  def primeFactors(requested : Long) : List[Long] = {
    primeFactors(requested,1,math.sqrt(requested).toLong,List()).sortWith((a,b)=> a < b)
  }
  
  @tailrec
  def primeFactors(requested : Long ,iterator : Long, end: Long, list : List[Long]) : List[Long] = {
    if(iterator > end) list
    else if(requested % iterator != 0) primeFactors(requested,iterator + 1,end,list)
    else primeFactors(requested,iterator+1,end,primes(requested,iterator) ::: list)
  }
  
  def primes(requested:Long, iterator:Long) : List[Long] = isPrime(iterator) ::: isPrime(requested / iterator)
  
  @tailrec
  def isPrime(numberToCheck:Long, division:Long=2) : List[Long] = {
    if(division > numberToCheck/2) List(numberToCheck)
    else if(numberToCheck % division == 0) List()
    else isPrime(numberToCheck,division+1)
  }
}

class Problem3Tests extends FunSuite with ShouldMatchers {
  test("Largest Prime Factor of 13195 should be 29") {
    Problem3.primeFactors(13195).last should be(29)
  }

  test("Largest Prime Factor of 600851475143 should be 6857") {
    Problem3.primeFactors(600851475143l).last should be(6857)
  }
}