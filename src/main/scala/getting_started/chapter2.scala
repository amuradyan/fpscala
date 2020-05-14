  package fpinscala
  package chapter2

object Chapter2 {

  def partial[A, B, C](a: A, f: (A, B) => C): B => C = b => f(a, b)

  // Excercise 2.1
  def Fib(n: Int): Int = {
    @annotation.tailrec
    def loop(a: Int, b: Int, step: Int): Int = {
      if (step == 0) a
      else loop(b, a + b, step - 1)
    }

    if(n <= 0) 0
    else loop(0, 1, n)
  }

  // Excercise 2.2
  def isSorted[A](as: Array[A], ord: (A, A) => Boolean): Boolean = {
    @annotation.tailrec
    def loop(idx: Int): Boolean = {
      if ((idx == as.length - 2)) ord(as(idx), as(idx + 1))
      else if (ord(as(idx), as(idx + 1))) loop(idx + 1) 
      else false
    }

    if (as.isEmpty || as.length == 1) true
    else loop(0)
  }

  // Excercise 2.3
  def curry[A, B, C](f: (A, B) => C): A => B => C = a => partial(a, f)
  
  // Excercise 2.4
  def uncurry[A, B, C](f: A => B => C): (A, B) => C = (a, b) => f(a)(b)

  // Excercise 2.5
  def compose[A, B, C](f: B => C, g: A => B): A => C = a => f(g(a))
}
