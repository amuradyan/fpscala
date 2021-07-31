package fpinscala
package chapter7
package par

import fpinscala.chapter3.lizt.Lizt
import java.util.concurrent.ExecutorService
import java.util.concurrent.Future
import java.util.concurrent.TimeUnit
import java.util.concurrent.Callable

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
      val af = pa(es)
      val bf = pb(es)

      UnitFuture(f(af.get(), bf.get()))
    }

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
}
