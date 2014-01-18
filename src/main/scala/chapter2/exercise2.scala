import scala.annotation.tailrec

object SortInquirer {
  // Given a sequence, determine that it's sorted according to a predicate
  // function taking two arguments.
  def isSorted[A](as: Array[A], gt: (A,A) => Boolean): Boolean = {
    @tailrec
    def check(previous: A, values: Array[A], sorted: Boolean): Boolean = {
      if (!sorted) sorted
      else if (values.length > 1) check(values.head, values.tail, gt(previous, values.head))
      else if (values.length == 1) gt(previous, values.head)
      else sorted
    }

    if (as.isEmpty) true
    else check(as.head, as.tail, true)
  }

  def main(args: Array[String]) = {
    assert(isSorted(Array("a", "b", "c", "d"), (x: String, y: String) => x < y))
    assert(!isSorted(Array("a", "b", "c", "d"), (x: String, y: String) => x > y))
    assert(isSorted[String](Array("aaaa", "bbb", "cc", "d"), (x, y) => x.length > y.length))
    assert(isSorted(Array(), (x: Int, y: Int) => x > y))
    assert(isSorted(Array(1), (x: Int, y: Int) => x > y))
  }
}
