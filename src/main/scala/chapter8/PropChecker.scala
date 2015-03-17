package fpinscala.propchecker

import scala.annotation.tailrec

import fpinscala.state._
import Prop._

case class Gen[+A](sample: State[RNG,A]) {
  def map[B](f: A => B): Gen[B] = ???

  def flatMap[B](f: A => Gen[B]): Gen[B] = {
    Gen(sample.flatMap(a => f(a).sample))
  }

  def listOfN(size: Gen[Int]): Gen[List[A]] = {
    size.flatMap(n => Gen(State.sequence(List.fill(n)(sample))))
  }
}

object Gen {
  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    Gen(State(RNG.nonNegativeInt).map(n => start + n % (stopExclusive - start)))

  def boolean: Gen[Boolean] = Gen(State(RNG.boolean))

  def listOf[A](a: Gen[A]): Gen[List[A]] = ???
  def listOfN[A](n: Int, a: Gen[A]): Gen[List[A]] =
    Gen(State.sequence(List.fill(n)(a.sample)))

  def union[A](g1: Gen[A], g2: Gen[A]) =
    boolean.flatMap(b => if (b) g1 else g2)

  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = {
    val threshold = g1._2.abs / (g1._2.abs + g2._2.abs)

    Gen(State(RNG.double).flatMap(d =>
      if (d < threshold) g1._1.sample else g2._1.sample))
  }
}

trait Prop {
  def check: Either[(FailedCase, SuccessCount), SuccessCount]
  // def &&(p: Prop): Prop = new Prop {
  //   def check = Prop.this.check && p.check
  // }
}

object Prop {
  type FailedCase = String
  type SuccessCount = Int

  def forAll[A](a: Gen[A])(f: A => Boolean): Prop = ???
}
