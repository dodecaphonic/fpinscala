import scala.annotation._

object Exercise1 {
  def fib(n: Long): Long = {
    @tailrec
    def go(a: Long, b: Long, m: Long): Long = {
      if (m < 1) a
      else if (m == 1) b
      else go(b, a + b, m - 1)
    }

    go(0, 1, n)
  }

  def main(args: Array[String]) = {
    assert(fib(0) == 0, "fib(0) should be 0")
    assert(fib(1) == 1, "fib(1) should be 1")
    assert(fib(2) == 1, "fib(2) should be 1")
    assert(fib(3) == 2, "fib(3) should be 2")
    assert(fib(4) == 3, "fib(4) should be 3")
    assert(fib(5) == 5, "fib(5) should be 5")
    assert(fib(6) == 8, "fib(6) should be 8")
    assert(fib(7) == 13, "fib(7) should be 13")
    println(fib(20))
  }
}
