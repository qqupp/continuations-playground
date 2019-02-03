package data.continuation.tailrec

import scala.util.control.TailCalls.{TailRec, done, tailcall}

final case class Continuation[A, R](k: (A => TailRec[R]) => TailRec[R]) { self =>
  def flatMap[B](f: A => Continuation[B, R]): Continuation[B, R] =
    Continuation(  br  =>  tailcall{
        k(  a => tailcall{ f(a).k(br) }   )
      }
    )

  def map[B](f: A => B): Continuation[B, R] =
    Continuation(  br  =>  tailcall{
        k(  a => tailcall{ br(f(a)) }   )
      }
    )

  def run(f: A => R): R = k( a => done(f(a)) ).result
}

object Continuation {
  def pure[A, R](value: A): Continuation[A, R] = Continuation(  f => tailcall{ f(value) }  )
}
