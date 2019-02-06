package example

import data.Tree._

object TreePlaygroung {
  def treeFromInput: Tree[Int] = ???
  val tree1: Tree[Int] = Node(Leaf(1), Leaf(2))
  val tree2: Tree[Int] = Node(Leaf(1), Leaf(3))

  val sameTrees: Boolean = tree1 == tree2

  val tree3: Tree[Int] = treeFromInput
  val tree4: Tree[Int] = treeFromInput

  val sameInput: Boolean = tree1 == tree2
}

object MapTreeNaive {

  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(a)    => Leaf(f(a))
    case Node(l, r) => Node(map(l)(f), map(r)(f))
  }


  def map_[A, B](t: Tree[A])(f: A => B): Tree[B] = {
    def mapping(tt: Tree[A]): Tree[B] = tt match {
      case Leaf(a) => Leaf(f(a))
      case Node(l, r) => Node(mapping(l), mapping(r))
    }

    mapping(t)
  }

}

object MapTreeContinuation {

  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = {

    def mapping(tt: Tree[A], k: Tree[B] => Tree[B]): Tree[B] = tt match {
      case Leaf(a)      => k(Leaf(f(a)))
      case Node(la, ra) =>
        mapping(la, lb =>
          mapping(ra, rb =>
            k(Node(lb, rb))
          )
        )
    }

    mapping(t, x => x)
  }

}


object MapTreeTowardsContDataType {

  def map_[A, B](t: Tree[A])(f: A => B): Tree[B] = {

    def mapping(tt: Tree[A]): (Tree[B] => Tree[B]) => Tree[B] = ???

    mapping(t)(x => x)
  }


  case class Cont[A, R](k: (A => R) => R) {
    def compose[B](f: A => Cont[B, R]): Cont[B, R] =
      Cont[B, R] { k1 =>
        def type_0:     (B => R) => R        = ???
        val type_1:     (A => R) => R        = k
        val type_2:     A => Cont[B, R]      = f
        val type_3:     A => (B => R) => R   = a => f(a).k
        val type_4:     B => R               = k1
        val type_5:     A => R               = a => f(a).k(k1)
        val type_6:     R                    = k( a => f(a).k(k1) )

        k( a => f(a).k(k1) )
      }
  }

  def pure[A, R](a: A) = Cont[A,R]( k => k(a) )


  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = {

    def mapping(tt: Tree[A]): Cont[Tree[B],Tree[B]] = tt match {
      case Leaf(a)      => pure(Leaf(f(a)))
      case Node(la, ra) =>
        mapping(la).compose( lb =>
          mapping(ra).compose( rb =>
            pure( Node(lb, rb) )
          )
        )
    }

    mapping(t).k(x => x)
  }

}

object MapTreeContinuationMonad {

  import data.continuation.basic.Continuation
  import data.continuation.basic.Continuation._

  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = {

    def mapping(tt: Tree[A]): Continuation[Tree[B], Tree[B]] = tt match {
      case Leaf(a)      => pure(Leaf(f(a)))
      case Node(la, ra) =>
        for {
          lb <- mapping(la)
          rb <- mapping(ra)
        } yield Node(lb, rb)

    }

    mapping(t).run(identity)
  }

}

object MapTreeContinuationTailrec {
  import scala.util.control.TailCalls._

  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = {

    def mapping(tt: Tree[A], k: Tree[B] => TailRec[Tree[B]]): TailRec[Tree[B]] = tt match {
      case Leaf(a)      => k(Leaf(f(a)))
      case Node(la, ra) =>
        mapping(la, lb => tailcall(
          mapping(ra, rb => tailcall(
            k(Node(lb, rb))
          ))
        ))
    }

    mapping(t, x => done(x)).result
  }

}

object MapTreeContinuationTailrecMonad {

  import data.continuation.tailrec.Continuation
  import data.continuation.tailrec.Continuation._

  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = {

    def mapping(tt: Tree[A]): Continuation[Tree[B], Tree[B]] = tt match {
      case Leaf(a)      => pure(Leaf(f(a)))
      case Node(la, ra) =>
        for {
          lb <- mapping(la)
          rb <- mapping(ra)
          v  <- pure(Node(lb, rb))
        } yield v

    }

    mapping(t).run(identity)
  }

}

