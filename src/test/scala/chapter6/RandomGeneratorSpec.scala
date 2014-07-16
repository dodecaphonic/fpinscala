package fpinscala.specs

import org.specs2.mutable._
import org.specs2.ScalaCheck
import fpinscala.state._

class RandomGeneratorSpec extends Specification with ScalaCheck {
  def defaultSimple = simple(42)
  def simple(seed: Long) = RNG.Simple(seed)

  "A Simple generator" should {
    "nextInt" in {
      val gen = defaultSimple
      val (n1, nextGen1) = gen.nextInt
      val (n2, nextGen2) = gen.nextInt
      n1 must beEqualTo(n2)
      nextGen1 must beEqualTo(nextGen2)
    }

    "nonNegativeInt" ! prop { (s: Long) => RNG.nonNegativeInt(simple(s))._1 >= 0 }

    "double" ! prop((s: Long) => {
      val (d, _) = RNG.double(simple(s))
      d >= 0.0 && d <= 1.0
    })

    "intDouble" ! prop((s: Long) => {
      val ((i, d), r) = RNG.intDouble(defaultSimple)
      i.toDouble != d
    })

    "doubleInt" ! prop((s: Long) => {
      val ((d, i), r) = RNG.doubleInt(defaultSimple)
      i.toDouble != d
    })

    "double3" ! prop((s: Long) => {
      val ((d, d1, d2), r) = RNG.double3(defaultSimple)
      d != d1 && d1 != d2
    })

    "ints" ! prop { (s: Long) =>
      val (l, r) = RNG.ints(10)(defaultSimple)
      l.length == 10 &&  l.distinct.length == 10
    }

    "nonNegativeInt" ! prop { (s: Long) => RNG.nonNegativeEven(defaultSimple)._1 % 2 == 0 }

    "doubleFromMap" ! prop((s: Long) => {
      val (d, _) = RNG.doubleFromMap(simple(s))
      d >= 0.0 && d <= 1.0
    })

    "doubleIntFromMap2" ! prop((s: Long) => {
      val ((d, i), r) = RNG.doubleIntFromMap2(defaultSimple)
      i.toDouble != d
    })

    "intDoubleFromMap2" ! prop((s: Long) => {
      val ((i, d), r) = RNG.intDoubleFromMap2(defaultSimple)
      i.toDouble != d
    })
  }
}
