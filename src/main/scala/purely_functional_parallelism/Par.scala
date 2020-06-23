package fpinscala
package chapter7
package par

import fpinscala.chapter3.lizt.Lizt

trait Par[A]

object Par {
  def unit[A](a: A): Par[A] = ???
  def lazyUnit[A](a: => A) = fork(unit(a))
  def run[A](pa: Par[A]): A = ???

  
  def sum(ints: Lizt[Int]): Int = {
    val length = Lizt.length(ints) 
    
    if (length <= 1 )
    Lizt.headOpshn(ints) getOrElse 0
    else {
      val (l, r) = Lizt.splitAt(ints, length / 2)
      val suml = Par.unit(sum(l))
      val sumr = Par.unit(sum(r))
      
      Par.run(suml) + Par.run(sumr)
    }
  }
  
  // Excercise 7.1
  def map2[A, B, C](pa: Par[A], pb: Par[B])(f: ( A, B) => C): Par[C] = ???
  
  def _sum(ints: Lizt[Int]): Par[Int] = {
    val length = Lizt.length(ints) 
    
    if (length <= 1 )
    Par.unit(Lizt.headOpshn(ints) getOrElse 0)
    else {
      val (l, r) = Lizt.splitAt(ints, length / 2)
      
      Par.map2(_sum(l), _sum(r)) (_ + _)
    }
  }  

  def fork[A](a: => Par[A]): Par[A] = ???

  def __sum(ints: Lizt[Int]): Par[Int] = {
    val length = Lizt.length(ints) 

    if (length <= 1 )
      Par.unit(Lizt.headOpshn(ints) getOrElse 0)
    else {
      val (l, r) = Lizt.splitAt(ints, length / 2)
      
      Par.map2(Par.fork(__sum(l)), Par.fork(__sum(r))) (_ + _)
    }
  }  

}