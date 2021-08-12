package fpinscala
package chapter7
package par

import fpinscala.chapter3.lizt.Lizt
import java.util.concurrent.ExecutorService
import java.util.concurrent.Future
import java.util.concurrent.TimeUnit
import java.util.concurrent.Callable
import fpinscala.chapter4.opshn.Opshn
import fpinscala.chapter4.opshn.Non
import fpinscala.chapter4.opshn.Sam

object Par {
  type Par[A] = ExecutorService => Future[A]

  private case class UnitFuture[A](value: A) extends Future[A] {
    def isDone: Boolean = true
    def get: A = value
    def get(timeout: Long, unit: TimeUnit): A = get
    def isCancelled: Boolean = false
    def cancel(evenIfRunning: Boolean): Boolean = false
  }

  def fork[A](a: => Par[A]): Par[A] = es => 
    es.submit(new Callable[A] {
      def call: A = a(es).get
    })

  def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a)

  def lazyUnit[A](a: => A) = fork(unit(a))

  def run[A](es: ExecutorService)(a: Par[A]): Future[A] = a(es)

  // Exercise 7.1
  def map2[A, B, C](pa: Par[A], pb: Par[B])(f: (A, B) => C): Par[C] =
    (es: ExecutorService) => {
      val (af, bf) = (pa(es), pb(es))

      // This implementation does not respect timeouts.
      // UnitFuture(f(af.get( ), bf.get()))
      
      Map2Future(af, bf, f)
    }

    // Exercise 7.3
    private case class Map2Future[A, B, C](fa: Future[A], fb: Future[B], f: (A, B) => C) extends Future[C] {
      @volatile var cache: Opshn[C] = Non
      def isDone: Boolean = !cache.isEmpty
      def isCancelled(): Boolean = fa.isCancelled() || fb.isCancelled()
      def cancel(evenIfRunning: Boolean): Boolean = fa.cancel(evenIfRunning) || fb.cancel(evenIfRunning)
      def get(): C = compute(Long.MaxValue)
      def get(timeout: Long, unit: TimeUnit): C = compute(TimeUnit.NANOSECONDS.convert(timeout, unit))

      def compute(timeout:Long): C = cache match {
        case Sam(value) => value
        case Non => {
          val faStartTime = System.nanoTime
          val a = fa.get(timeout, TimeUnit.NANOSECONDS)
          val faDuration = System.nanoTime - faStartTime
          val b = fb.get(timeout - faDuration, TimeUnit.NANOSECONDS)
          val res = f(a, b)
          cache = Sam(res)
          res
        }
      }
    }

  // Exercise 7.4
  def asyncF[A, B](f: A => B): A => Par[B] = (a: A) => Par.lazyUnit(f(a))

  def sum(ints: Lizt[Int]): Par[Int] = {
    val length = Lizt.length(ints)

    if (length <= 1)
      Par.unit(Lizt.headOpshn(ints) getOrElse 0)
    else {
      val (l, r) = Lizt.splitAt(ints, length / 2)

      Par.map2(sum(l), sum(r))(_ + _)
    }
  }

  def lazy_sum(ints: Lizt[Int]): Par[Int] = {
    val length = Lizt.length(ints)

    if (length <= 1)
      Par.unit(Lizt.headOpshn(ints) getOrElse 0)
    else {
      val (l, r) = Lizt.splitAt(ints, length / 2)

      Par.map2(Par.fork(lazy_sum(l)), Par.fork(lazy_sum(r)))(_ + _)
    }
  }

  def map[A, B](pA: Par[A])(f: A => B): Par[B] = 
    map2(pA, Par.unit(()))((a, _) => f(a))
}
