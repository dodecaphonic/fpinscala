package fpinscala.specs

import org.specs2.mutable._
import fpinscala.laziness._

class StreamSpec extends Specification {
  "A Stream" should {
    val stream = Stream(1, 2, 3, 4)

    "spit out its head as an Option" in {
      stream.headOption must beEqualTo(Some(1))
      Empty.headOption must beEqualTo(None)
    }

    "be convertible to a List" in {
      stream.toList must beEqualTo(List(1, 2, 3, 4))
      Empty.toList must beEqualTo(Nil)
    }

    "allow user to take first n values" in {
      stream.take(2).toList must beEqualTo(Stream(1, 2).toList)
      stream.take(55).toList must beEqualTo(stream.toList)
      Empty.take(1337) must beEqualTo(Empty)
    }

    "allow user to drop first n values" in {
      stream.drop(2).toList must beEqualTo(Stream(3, 4).toList)
      stream.drop(55).toList must beEqualTo(Nil)
      Empty.drop(1337) must beEqualTo(Empty)
    }

    "takeWhile" in {
      stream.takeWhile(_ < 3).toList must beEqualTo(Stream(1, 2).toList)
      stream.takeWhile(_ > 2000) must beEqualTo(Empty)
    }

    "exists" in {
      stream.exists(_ == 2) must beTrue
      stream.exists(_ == 123) must beFalse
      stream.existsFromFoldRight(_ == 2) must beTrue
      stream.existsFromFoldRight(_ == 123) must beFalse
    }

    "takeWhileFromFoldRight" in {
      stream.takeWhileFromFoldRight(_ < 3).toList must beEqualTo(Stream(1, 2).toList)
      stream.takeWhileFromFoldRight(_ < 2).toList must beEqualTo(Stream(1).toList)
      stream.takeWhileFromFoldRight(_ > 2000) must beEqualTo(Empty)
    }

    "map" in {
      stream.map(_ * 10).toList must beEqualTo(List(10, 20, 30, 40))
      Stream.empty[Int].map(_ * 10) must beEqualTo(Empty)
      Stream(1).map(_ - 1).toList must beEqualTo(List(0))
    }

    "filter" in {
      stream.filter(_ % 2 == 0).toList must beEqualTo(List(2, 4))
      stream.filter(_ % 2 != 0).toList must beEqualTo(List(1, 3))
      Stream.empty[Int].map(_ % 2 == 0) must beEqualTo(Empty)
    }

    "append" in {
      stream.append(Stream(5, 6)).toList must beEqualTo(List(1, 2, 3, 4, 5, 6))
    }

    "flatMap" in {
      stream.flatMap(v => Stream(v)).toList must beEqualTo(List(1, 2, 3, 4))
    }

    "constant" in {
      Stream.constant(2).forAll(_ % 2 != 0) must beFalse
    }

    "from" in {
      Stream.from(10).take(5).toList must beEqualTo(List(10, 11, 12, 13, 14))
    }

    "fibs" in {
      Stream.fibs.take(5).toList must beEqualTo(List(0, 1, 1, 2, 3))
    }

    "unfold" in {
      Stream.unfold(0)(s => Some(s, s + 1)).take(5).toList must beEqualTo(List(0, 1, 2, 3, 4))
    }

    "from2" in {
      Stream.from2(0).take(5).toList must beEqualTo(List(0, 1, 2, 3, 4))
    }

    "fibs2" in {
      Stream.fibs2.take(5).toList must beEqualTo(List(0, 1, 1, 2, 3))
    }

    "constant2" in {
      Stream.constant2("a").take(5).toList must beEqualTo(List("a", "a", "a", "a", "a"))
    }

    "ones2" in {
      Stream.ones2.take(5).toList must beEqualTo(List(1, 1, 1, 1, 1))
    }

    "map2" in {
      Stream(1, 2, 3).map2(_ * 2).toList must beEqualTo(List(2, 4, 6))
    }

    "take2" in {
      Stream(1, 2, 3).take2(2).toList must beEqualTo(List(1, 2))
    }

    "takeWhile2" in {
      Stream(1, 2, 3, 4, 5).takeWhile(_ < 4).toList must beEqualTo(List(1, 2, 3))
    }

    "zipWith" in {
      Stream(1, 2, 3).zipWith(Stream(4, 5, 6))((a, b) => b - a).toList must beEqualTo(List(3, 3, 3))
    }

    "zipAll" in {
      Stream(1, 2, 3).zipAll(Stream(4, 5)).toList must beEqualTo(List((Some(1), Some(4)), (Some(2), Some(5)), (Some(3), None)))
    }

    "startsWith" in {
      Stream(1, 2, 3).startsWith(Stream(1, 2)) must beTrue
    }

    "tails" in {
      Stream(1, 2, 3).tails.map(_.toList).toList must beEqualTo(List(List(1, 2, 3), List(2, 3), List(3), List()))
    }

    "tails2" in {
      Stream(1, 2, 3).tails.map(_.toList).toList must beEqualTo(List(List(1, 2, 3), List(2, 3), List(3), List()))
    }
  }
}
