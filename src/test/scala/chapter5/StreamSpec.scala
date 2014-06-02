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
  }
}
