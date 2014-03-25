package fpinscala

import scala.{Option => _, Either => _, _}

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case None    => None
    case Some(v) => Some(f(v))
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case None    => default
    case Some(v) => v
  }

  def flatMap[B](f: A => Option[B]): Option[B] =
    map(f).getOrElse(None)

  def orElse[B >: A](ob: => Option[B]): Option[B] =
    this.map(Some(_)).getOrElse(ob)

  def filter(f: A => Boolean): Option[A] =
    flatMap(a => if (f(a)) Some(a) else None)
}

case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {
  def mean(xs: Seq[Double]): Option[Double] = {
    if (xs.isEmpty) None
    else Some(xs.sum/xs.length)
  }

  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs).flatMap(m => mean(xs.map(x => Math.pow(x - m, 2))))

  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    a.flatMap(aa => b map (bb => f(aa, bb)))

  def sequence[A](a: List[Option[A]]): Option[List[A]] =
    a.foldRight[Option[List[A]]](Some(Nil))((x, y) => map2(x, y)(_ :: _))

  def sequenceRecursive[A](a: List[Option[A]]): Option[List[A]] = a match {
    case Nil => Some(Nil)
    case h :: t => h.flatMap(hh => sequence(t).map(hh :: _))
  }

  def traverse[A,B](a: List[A])(f: A => Option[B]): Option[List[B]] = a match {
    case Nil => Some(Nil)
    case h :: t => map2(f(h), traverse(t)(f))(_ :: _)
  }

  def sequenceFromTraverse[A](a: List[Option[A]]): Option[List[A]] =
    traverse(a)(v => v)

  def Try[A](a: => A): Option[A] =
    try Some(a)
    catch { case e: Exception => None }
}

object Runner {
  def main(args: Array[String]) = {
    println(Option.mean(List(4, 36, 45, 50, 75)))
    println(Option.variance(List(4, 36, 45, 50, 75)))
    println(Option.sequence(List(Some(1), Some(2), Some(3))))
    println(Option.sequence(List(Some(1), Some(2), None)))
    println(Option.sequenceFromTraverse(List(Some(1), Some(2), Some(3))))
    println(Option.sequenceFromTraverse(List(Some(1), Some(2), None)))
    println(Option.traverse(List(1, 2, 3))(Some(_)))
    println(Option.traverse(List(1, 2, 3))(v => if (v == 2) None else Some(v)))
  }
}
