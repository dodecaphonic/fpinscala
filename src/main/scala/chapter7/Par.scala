package fpinscala.parallel

import java.util.concurrent.Callable
import java.util.concurrent.ExecutorService
import java.util.concurrent.Future
import java.util.concurrent.TimeUnit

object Par {
  type Par[A] = ExecutorService => Future[A]

  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

  def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a)

  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone = true
    def get(timeout: Long, units: TimeUnit) = get
    def isCancelled = false
    def cancel(evenIfRunning: Boolean): Boolean = false
  }

  def map2[A,B,C](a: Par[A], b: Par[B])(f: (A,B) => C): Par[C] =
    (es: ExecutorService) => {
      val af = a(es)
      val bf = b(es)
      UnitFuture(f(af.get, bf.get))
    }

  def fork[A](a: => Par[A]): Par[A] =
    es => es.submit(new Callable[A] {
      def call = a(es).get
    })

  def asyncF[A,B](f: A => B): A => Par[B] =
    a => fork(unit(f(a)))

  def map[A,B](pa: Par[A])(f: A => B): Par[B] =
    map2(pa, unit(()))((a, _) => f(a))

  def sortPar(parList: Par[List[Int]]) = map(parList)(_.sorted)

  // My take on it
  def sequenceNaive[A](l: List[Par[A]]): Par[List[A]] =
    l.foldRight[Par[List[A]]](unit(Nil))((a, acc) => map2(a, acc)(_ :: _))

  // Got it from the answers. Pretty cool.
  def sequenceBalanced[A](s: IndexedSeq[Par[A]]): Par[IndexedSeq[A]] =
    if (s.isEmpty) unit(Vector())
    else if (s.length == 1) map(s.head)(v => Vector(v))
    else {
      val (l, r) = s.splitAt(s.length / 2)
      map2(sequenceBalanced(l), sequenceBalanced(r))(_ ++ _)
    }

  def sequence[A](l: List[Par[A]]): Par[List[A]] =
    map(sequenceBalanced(l.toIndexedSeq))(as => as.toList)

  def parMap[A,B](l: List[A])(f: A => B): Par[List[B]] = fork {
    val fbs: List[Par[B]] = l.map(asyncF(f))
    sequence(fbs)
  }

  def parFilter[A](l: List[A])(f: A => Boolean): Par[List[A]] = {
    val pars: List[Par[List[A]]] =
      l.map(asyncF((a: A) => if(f(a)) List(a) else List()))
    map(sequence(pars))(_.flatten)
  }

  def sum(ints: IndexedSeq[Int]): Par[Int] =
    if (ints.size <= 1)
      Par.unit(ints.headOption getOrElse 0)
    else {
      val (l, r) = ints.splitAt(ints.length/2)
      Par.map2(sum(l), sum(r))(_ + _)
    }

  def equal[A](es: ExecutorService)(a: Par[A], b: Par[A]): Boolean =
    a(es).get == b(es).get

  def delay[A](fa: => Par[A]): Par[A] =
    es => fa(es)

  def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    es =>
      if (run(es)(cond).get) t(es)
      else f(es)

  def choiceN[A](n: Par[Int])(choices: List[Par[A]]): Par[A] =
    es =>
      map(n)(i => choices(i)(es).get)(es)

  def choiceFromChoiceN[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    choiceN(map(cond)(v => if (v) 0 else 1))(List(t, f))

  def choiceMap[K,V](key: Par[K])(choices: Map[K,Par[V]]): Par[V] =
    es =>
      map(key)(k => choices(k)(es).get)(es)

  def chooser[A,B](pa: Par[A])(choices: A => Par[B]): Par[B] =
    es =>
      map(pa)(v => choices(v)(es).get)(es)

  def choiceFromChooser[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    chooser(cond)(v => if(v) t else f)

  def choiceNFromChooser[A](n: Par[Int])(choices: List[Par[A]]): Par[A] =
    chooser(n)(i => choices(i))

  def flatMap[A,B](a: Par[A])(f: A => Par[B]): Par[B] =
    es =>
      map(a)(v => f(v)(es).get)(es)

  def joinFromFlatMap[A](a: Par[Par[A]]): Par[A] =
    flatMap(a)(b => b)

  def join[A](a: Par[Par[A]]): Par[A] =
    es => run(es)(run(es)(a).get())

  def flatMapFromJoin[A,B](pa: Par[A])(f: A => Par[B]): Par[B] =
    join(map(pa)(f))
}
