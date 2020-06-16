package fpinscala
package chapter6
package steyt

import fpinscala.chapter3.lizt.Conz
import fpinscala.chapter3.lizt.Nill
import fpinscala.chapter3.lizt.Lizt

case class Steyt[S, +A] (run: S => (A, S)) {
  // Excercise 6.10 (b)
  def map[B](f: A => B): Steyt[S, B] = Steyt { s => 
    val (a, _s) = run(s)
    (f(a), _s)
  }

  // Excercise 6.10 (c)
  def map2[B, C](sb: Steyt[S, B])(f: (A, B) => C): Steyt[S, C] = Steyt { s => 
    val (a, _s) = run(s)
    val (b, __s) = sb.run(_s)

    (f(a, b), __s)
  }

  // Excercise 6.10 (d)
  def flatMap[B](f: A => Steyt[S, B]): Steyt[S, B] = Steyt { s => 
    val (a, _s) = run(s)
    f(a).run(_s)
  }
}

object Steyt {
  // Excercise 6.10 (a)
  def unit[A, S](a: A): Steyt[S, A] = Steyt((a, _))

  // Excercise 6.10 (e)
  def sequence[A, S](sa: Lizt[Steyt[S, A]]): Steyt[S, Lizt[A]] = 
    Lizt.foldRightViaFoldLeft(sa, unit[Lizt[A], S](Nill)) { (a, acc) =>
      a.map2(acc)(Conz(_, _))
    }

  def get[S]: Steyt[S, S] = Steyt(s => (s, s))
  
  def set[S](a: S): Steyt[S, Unit] = Steyt(_ => ((), a))

  def modify[S](f: S => S): Steyt[S, Unit] = for {
    s <- get
    _ <- set(f(s))
  } yield ()
}
