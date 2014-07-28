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

    "double3" ! prop((s: Long) => {
      val ((d, d1, d2), r) = RNG.double3(simple(s))
      d != d1 && d1 != d2
    })

    "ints" ! prop { (s: Long) =>
      val (l, r) = RNG.ints(10)(simple(s))
      l.length == 10 &&  l.distinct.length == 10
    }

    "nonNegativeInt" ! prop { (s: Long) => RNG.nonNegativeEven(simple(s))._1 % 2 == 0 }

    "doubleFromMap" ! prop((s: Long) => {
      val (d, _) = RNG.doubleFromMap(simple(s))
      d >= 0.0 && d <= 1.0
    })

    "doubleIntFromMap2" ! prop((s: Long) => {
      val ((d, i), r) = RNG.doubleIntFromMap2(simple(s))
      i.toDouble != d
    })

    "intDoubleFromMap2" ! prop((s: Long) => {
      val ((i, d), r) = RNG.intDoubleFromMap2(simple(s))
      i.toDouble != d
    })

    "sequence" in {
      val fs = List(RNG.unit(10), RNG.unit(20), RNG.unit(40))
      val ss = RNG.sequence(fs)
      val (sfs, rng) = ss(defaultSimple)
      sfs must beEqualTo(List(10, 20, 40))
      rng must beEqualTo(defaultSimple)
    }

    "intsFromSequence" in {
      val ss = RNG.intsFromSequence(5)
      val (fs, rng) = ss(defaultSimple)
      fs.length must beEqualTo(5)
      fs.distinct.length must beEqualTo(5)
    }

    "mapFromFlatMap" in {
      val rng = defaultSimple
      RNG.doubleFromMapFromFlatMap(rng) must beEqualTo(RNG.doubleFromMap(rng))
    }

    "intDoubleFromMap2FromFlatMap" ! prop((s: Long) => {
      val rng = defaultSimple
      RNG.intDoubleFromMap2FromFlatMap(rng) must beEqualTo(RNG.intDoubleFromMap2(rng))
    })
  }
}
