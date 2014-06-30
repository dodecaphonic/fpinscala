package fpinscala.laziness

import scala.collection.immutable._
import Stream._

sealed trait Stream[+A] {
  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, t) => Some(h())
  }

  def toList: List[A] = this match {
    case Empty => Nil
    case Cons(h, t) => h() :: t().toList
  }

  def take(n: Int): Stream[A] =
    if (n > 0) this match {
      case Cons(h, t) => cons(h(), t().take(n -1))
      case _ => Stream.empty
    }
    else Stream()

  def drop(n: Int): Stream[A] =
    if (n > 0) this match {
      case Cons(h, t) => t().drop(n - 1)
      case _ => Stream.empty
    }
    else this

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) => if (p(h())) cons(h(), t().takeWhile(p)) else Stream.empty
    case _ => Stream.empty
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
    case _ => z
  }

  def exists(p: A => Boolean): Boolean = this match {
    case Cons(h, t) => p(h()) || t().exists(p)
    case _ => false
  }

  def existsFromFoldRight(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b)

  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a, b) => p(a) && b)

  def takeWhileFromFoldRight(p: A => Boolean): Stream[A] =
    foldRight(Empty:Stream[A])((a, b) => if (p(a)) cons(a, b) else empty)

  def headOptionFromFoldRight: Option[A] =
    foldRight(None:Option[A])((h, t) => Some(h))

  def map[B](f: A => B): Stream[B] =
    foldRight(empty[B])((h, t) => cons(f(h), t))

  def filter(f: A => Boolean): Stream[A] =
    foldRight(empty[A])((h, t) =>
      if (f(h)) cons(h, t)
      else t)

  def append[B>:A](s: => Stream[B]): Stream[B] =
    foldRight(s)((h, t) => cons(h, t))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty[B])((h, t) => f(h) append t)

  def map2[B](f: A => B): Stream[B] = unfold(this) {
    case Cons(h, t) => Some((f(h()), t()))
    case _ => None
  }

  def take2(n: Int): Stream[A] = unfold((this, n)) {
    case (Cons(h, t), n) if n > 0 => Some((h(), (t(), n - 1)))
    case _ => None
  }

  def takeWhile2(f: A => Boolean): Stream[A] = unfold(this) {
    case Cons(h, t) if (f(h())) => Some((h(), t()))
    case _ => None
  }

  def zipWith[B, C](s: Stream[B])(f: (A, B) => C): Stream[C] = unfold((this, s)) {
    case (Empty, _) => None
    case (_, Empty) => None
    case (Cons(h1, t1), Cons(h2, t2)) => Some((f(h1(), h2()), (t1(), t2())))
  }

  // This is much uglier than the official answer, but I'm keeping it as a record of my naivete.
  def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] = unfold((this, s2)) {
    case (Empty, Empty) => None
    case (Empty, Cons(h, t)) => Some((None, Some(h())), (empty, t()))
    case (Cons(h, t), Empty) => Some((Some(h()), None), (t(), empty))
    case (Cons(h1, t1), Cons(h2, t2)) => Some((Some(h1()), Some(h2())), (t1(), t2()))
  }

  // Was on the right track, but didn't occur to me to exhaust the secondary stream first.
  // Looked at the answer.
  def startsWith[A](s: Stream[A]): Boolean =
    zipAll(s).takeWhile(!_._2.isEmpty) forAll {
      case (h1, h2) => h1 == h2
    }

  // My take on it, forgetting unfold exists
  def tails: Stream[Stream[A]] = this match {
    case Cons(_, _) => cons(this, drop(1).tails)
    case Empty => Stream(empty)
  }

  // Implemented via unfold, per official answer
  def tails2: Stream[Stream[A]] = unfold(this) {
    case Empty => None
    case s => Some((s, s drop 1))
  } append(Stream(empty))
}

case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

  def constant[A](v: A): Stream[A] = Stream.cons(v, constant(v))

  def from(n: Int): Stream[Int] = Stream.cons(n, from(n + 1))

  def fibs: Stream[Int] = {
    def go(n0: Int, n1: Int): Stream[Int] =
      cons(n0, go(n1, n0 + n1))
    go(0, 1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case Some((v, s)) => cons(v, unfold(s)(f))
    case None => empty
  }

  def from2(n: Int): Stream[Int] =
    unfold(n)(n => Some((n, n + 1)))

  def fibs2: Stream[Int] =
    unfold((0, 1)) { case (n0, n1) => Some((n0, (n1, n0 + n1))) }

  def constant2[A](v: A): Stream[A] =
    unfold(v)(v => Some((v, v)))

  def ones2: Stream[Int] =
    unfold(1)(n => Some((n, n)))
}
