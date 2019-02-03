package example

import Tree._


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

object TreeNaive {

  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(a)    => Leaf(f(a))
    case Node(l, r) => Node(map(l)(f), map(r)(f))
  }

}

