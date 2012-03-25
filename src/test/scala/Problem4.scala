import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import scala.annotation.tailrec

object Problem4 {
  def palindromes(max : Int) = {
    for {
      outer <- 2 to max
      inner <- 1 to (outer - 1) 
      val num = outer*inner
      if isPalindrome(num.toString())
    } yield num
  }.sortWith((a,b) => a>b)
  
  @tailrec
  def isPalindrome(characters : String, position:Int = 0) : Boolean = 
    if(position > characters.size/2) true
    else if(characters(position) != characters((characters.size-position)-1)) false
    else isPalindrome(characters, position + 1)
}

class Problem4Tests extends FunSuite with ShouldMatchers {
  test("The largest palindrome from a 2 digit product should be 9009") {
    Problem4.palindromes(99).head should be (9009)
  }
  
  test("The largest palindrome from a 3 digit product should be 906609") {
    Problem4.palindromes(999).head should be (906609)
  }
}