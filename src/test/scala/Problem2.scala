import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

object Problem2 {
  def sumOfEvenFib(max : Int) = {
    val f = new Fib(1,2,max)
    f.filter(_ % 2==0).sum
  }
}

class Fib(a:Int, b:Int,max:Int) extends Stream[Int] {
  override def head = a
  override def tail = new Fib(b,a+b,max)
  protected def tailDefined = a<max
  override def isEmpty = !tailDefined
}

class Problem2Tests extends FunSuite with ShouldMatchers {
  test("Sum of even fib numbers less than 100") {
    Problem2.sumOfEvenFib(100) should be(44)
  }

  test("Sum of even fib numbers less than 4000000") {
    Problem2.sumOfEvenFib(4000000) should be(4613732)
  }
}