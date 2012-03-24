import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import scala.annotation.tailrec

object Problem3 {
  def primeFactors(requested : Long) : List[Long] = {
    primeFactors(requested,1,math.sqrt(requested).toLong, List()).sortWith((a,b)=> a < b)
  }
  
  @tailrec
  def primeFactors(requested : Long ,division : Long, end: Long, list : List[Long]) : List[Long] = {
    if(division > end) list
    else if(requested % division != 0) primeFactors(requested,division + 1,end,list)
    else primeFactors(requested,division+1,end,primes(requested,division) ::: list)
  }
  
  def primes(requested:Long, division:Long) : List[Long] = isPrime(division) ::: isPrime(requested / division)
  
  def isPrime(number :Long) :List[Long] = {
    if(primeRec(2,number)) List(number)
    else List()
  }

  @tailrec
  def primeRec(division:Long, number:Long) : Boolean = {
    if(number % division == 0) false
    else if(division > number/2) true
    else primeRec(division+1,number)
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