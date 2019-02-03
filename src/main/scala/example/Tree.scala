package example

import data.Tree._

object TreeNaive {

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

object TreeCont {

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


object TowardContDataType {

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

  def pure[A, R](a: A) = Cont[A,R](f => f(a))


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

object TreeWContinuation {

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