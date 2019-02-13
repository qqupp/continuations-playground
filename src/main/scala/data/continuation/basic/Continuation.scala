package data.continuation.basic

final case class Continuation[A, R](k: (A => R) => R) { self =>
  def flatMap[B](f: A => Continuation[B, R]): Continuation[B, R] =
    Continuation(   br  =>  k( a => f(a).k(br) )   )

  def map[B](f: A => B): Continuation[B, R] =
    Continuation(   br  =>  k( a => br(f(a)) )   )

  def run(f: A => R): R = k( a => f(a) )
}

object Continuation {
  def pure[A, R](value: A): Continuation[A, R] = Continuation( k => k(value) )
}
