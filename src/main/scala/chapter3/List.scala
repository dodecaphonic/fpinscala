// package fpinscala.datastructures
import scala.annotation._

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def head[A](l: List[A]): Option[A] = l match {
    case Nil => None
    case Cons(x, _) => Some(x)
  }

  def tail[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(_, ys) => ys
  }

  def setHead[A](head: A, l: List[A]): List[A] = l match {
    case Nil => Cons(head, Nil)
    case _   => Cons(head, tail(l))
  }

  def drop[A](l: List[A], n: Int): List[A] =
    if (n == 0) l
    else drop(tail(l), n - 1)

  def dropWhile[A](l: List[A])(f: A => Boolean): List[A] = l match {
    case Nil => l
    case Cons(x, xs) => if (f(x)) dropWhile(xs)(f) else l
  }

  def append[A](a1: List[A], a2: List[A]): List[A] = a1 match {
    case Nil => a2
    case Cons(h,t) => Cons(h, append(t, a2))
  }

  def init[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(x, Nil) => Nil
    case Cons(x, xs) => Cons(x, init(xs))
  }

  def foldRight[A,B](l: List[A], z: B)(f: (A, B) => B): B = l match {
    case Nil => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }

  def length[A](l: List[A]): Int =
    foldRight(l, 0)((x, y) => y + 1)

  def sum2(ints: List[Int]) = foldRight(ints, 0)((x, y) => x + y)

  def product2(ints: List[Int]) = foldRight(ints, 1)((x, y) => x * y)

  @tailrec
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
  }

  def foldRightFromFoldLeft[A,B](l: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(reverse(l), z)((x, y) => f(y, x))

  def sum3(ints: List[Int]) = foldLeft(ints, 0)((x, y) => x + y)

  def product3(ints: List[Int]) = foldLeft(ints, 1)((x, y) => x * y)

  def reverse[A](l: List[A]) =
    foldLeft(l, Nil:List[A])((t, h) => append(List(h), t))

  def append2[A](a1: List[A], a2: List[A]): List[A] =
    foldRight(a1, a2)((v, l) => Cons(v, l))

  def append3[A](a1: List[A], a2: List[A]): List[A] =
    foldRight2(a1, a2)((v, l) => Cons(v, l))

  // Tried this one many times, cheated and looked at the answer.
  def concat[A](ll: List[List[A]]): List[A] =
    foldLeft(ll, Nil:List[A])(append)
}
