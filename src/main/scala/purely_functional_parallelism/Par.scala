package fpinscala
package chapter7
package par

import java.util.concurrent.ExecutorService
import java.util.concurrent.TimeUnit
import java.util.concurrent.Callable
import java.util.concurrent.Future
import fpinscala.chapter3.lizt.Lizt
import fpinscala.chapter3.lizt.Nill
import fpinscala.chapter3.lizt.Conz
import fpinscala.chapter3.lizt.Lizt.filter

object Par:
  opaque type Par[A] = ExecutorService => Future[A]

  private case class UnitFuture[A](get: A) extends Future[A]:
    def isDone = true
    def get(timeout: Long, units: TimeUnit): A = get
    def isCancelled = false
    def cancel(evenIfRunning: Boolean) = false

  def unit[A](a: A): Par[A] = es => UnitFuture(a)

  def fork[A](a: => Par[A]): Par[A] =
    es =>
      es.submit(new Callable[A] {
        def call = a(es).get()
      })

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  def asyncF[A, B](f: A => B): A => Par[B] =
    a => lazyUnit(f(a))

  extension [A](pa: Par[A])
    def run(es: ExecutorService): Future[A] =
      pa(es)

  extension [A](pa: Par[A])
    def map2[B, C](pb: Par[B])(f: (A, B) => C): Par[C] =
      (es: ExecutorService) =>
        val futureA = pa(es)
        val futureB = pb(es)
        UnitFuture(f(futureA.get, futureB.get))

  def mapErku[A, B, C](pa: Par[A], pb: Par[B])(f: (A, B) => C): Par[C] =
    (es: ExecutorService) =>
      val futureA = pa(es)
      val futureB = pb(es)
      UnitFuture(f(futureA.get, futureB.get))

  extension [A](pa: Par[A])
    def map[B](f: A => B) =
      pa.map2(unit(()))((a, _) => f(a))

  extension [A](pa: Par[A])
    def map2WithTimeout[B, C](pb: Par[B])(f: (A, B) => C): Par[C] =
      (es: ExecutorService) =>
        new Future[C]:
          private val futureA = pa(es)
          private val futureB = pb(es)
          @volatile private var cache: Option[C] = None

          def isDone(): Boolean = cache.isDefined
          def get(): C = get(Long.MaxValue, TimeUnit.NANOSECONDS)

          def get(timeout: Long, units: TimeUnit): C =
            val timenanos = TimeUnit.NANOSECONDS.convert(timeout, units)
            val started = System.nanoTime()
            val a = futureA.get(timenanos, TimeUnit.NANOSECONDS)
            val elapsed = System.nanoTime() - started
            val b = futureB.get(timeout - elapsed, TimeUnit.NANOSECONDS)
            val c = f(a, b)
            cache = Some(c)
            c

          def isCancelled(): Boolean = futureA.isCancelled() || futureB.isCancelled()

          def cancel(evenIfRunning: Boolean): Boolean =
            futureA.cancel(evenIfRunning) || futureB.cancel(evenIfRunning)

  def sum(ints: Lizt[Int]): Par[Int] =
    val elements = Lizt.length(ints)
    if elements < 2 then Par.unit(Lizt.headOpshn(ints).getOrElse(0))
    else
      val (l, r) = Lizt.splitAt(ints, elements / 2)
      mapErku(fork(sum(l)), fork(sum(r)))(_ + _)

  def sortPar(lizt: Par[Lizt[Int]]): Par[Lizt[Int]] =
    lizt.map(Lizt.sort(_)(_ < _))

  def parMap[A, B](ps: Lizt[A])(f: A => B): Par[Lizt[B]] =
    sequence(Lizt.map(ps)(asyncF(f)))

  def sequence[A](liztOfPars: Lizt[Par[A]]): Par[Lizt[A]] =
    val zero = unit(Lizt[A]())
    Lizt
      .foldLeft(liztOfPars, zero) { (accumulator, computation) =>
        accumulator
          .map2(computation) { (previous, current) =>
            Lizt.append(previous, Lizt(current))
          }
      }

  def parFilter[A](as: Lizt[A])(f: A => Boolean): Par[Lizt[A]] =
    val zero = unit(Lizt[A]())

    Lizt
      .foldLeft(Lizt.map(as)(lazyUnit), zero) { (accumulator, computation) =>
        accumulator
          .map2(computation) { (previous, current) =>
            if f(current) then Lizt.append(previous, Lizt(current))
            else previous
          }
      }
