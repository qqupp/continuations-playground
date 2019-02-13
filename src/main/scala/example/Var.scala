package example

import scala.annotation.tailrec

object CPSisTailrec extends App {

  @tailrec
  def f(k: Unit => Unit): Unit = f(k)


  def g(k: Unit => Unit): Unit = g(x => g(k))
//  @tailrec
//    [error] /Users/ppi11/Development/continuations-playground/src/main/scala/example/Var.scala:11:40: could not optimize @tailrec annotated method g: it contains a recursive call not in tail position
//    [error]   def g(k: Int => Int): Int = g(x => g(k))
//    [error]

}