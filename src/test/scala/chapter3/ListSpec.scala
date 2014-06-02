package fpinscala.specs

import org.specs2.mutable._
import fpinscala.collections._

class ListSpec extends Specification {
  val l = List(1, 2, 3, 4, 5)

  "A List" should {
    "be created with a unit method" in {
      l must beEqualTo(Cons(1, Cons(2, Cons(3, Cons(4, Cons(5, Nil))))))
    }

    "have the reverse of its reverse be itself" in {
      List.reverse(List.reverse(l)) must beEqualTo(l)
    }

    "be able to have its head changed" in {
      List.setHead(10, l) must beEqualTo(List(10, 2, 3, 4, 5))
    }

    "return an Option when getting its head" in {
      List.head(List(1, 2)) must beEqualTo(Some(1))
      List.head(Nil) must beEqualTo(None)
    }

    "drop values" in {
      List.drop(l, 3) must beEqualTo(List(4, 5))
      List.drop(l, 5) must beEqualTo(Nil)
    }

    "drop values while a predicate matches" in {
      List.dropWhile(l)(v => v < 4) must beEqualTo(List(4, 5))
      List.dropWhile(l)(v => v > 1337) must beEqualTo(l)
      List.dropWhile(l)(v => v > 0) must beEqualTo(Nil)
    }

    "fold from the right" in {
      List.foldRight(l, 0)(_ + _) must beEqualTo(15)
      List.foldRight(l, Nil:List[Int])((v, nl) => Cons(v * 2, nl)) must beEqualTo(List(2, 4, 6, 8, 10))
      List.foldRight(Nil, 0)((a: Int, b: Int) => a + b) must beEqualTo(0)
    }

    "fold from the left" in {
      List.foldLeft(l, 0)(_ + _) must beEqualTo(15)
      List.foldLeft(l, Nil:List[Int])((nl, v) => Cons(v * 2, nl)) must beEqualTo(List(10, 8, 6, 4, 2))
      List.foldLeft(Nil, 0)((a: Int, b: Int) => a + b) must beEqualTo(0)
    }

    "fold from the right using foldLeft" in {
      List.foldRightFromFoldLeft(l, 0)(_ + _) must beEqualTo(15)
      List.foldRightFromFoldLeft(l, Nil:List[Int])((v, nl) => Cons(v * 2, nl)) must beEqualTo(List(2, 4, 6, 8, 10))
      List.foldRightFromFoldLeft(Nil, 0)((a: Int, b: Int) => a + b) must beEqualTo(0)
    }

    "append a list to another" in {
      List.append(l, List(10, 20, 30, 40, 50)) must beEqualTo(List(1, 2, 3, 4, 5, 10, 20, 30, 40, 50))
    }

    "generate a new list from a function (map)" in {
      List.map(l)(_ * 200) must beEqualTo(List(200, 400, 600, 800, 1000))
    }

    "filter values" in {
      List.filter(l)(_ % 2 == 0) must beEqualTo(List(2, 4))
    }
  }
}
