package example


sealed trait Tree[T] extends Product with Serializable
final case class Node[T](left: Tree[T], right: Tree[T]) extends Tree[T]
final case class Leaf[T](value: T) extends Tree[T]

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

object TreeUtils {

  def deepTree: Tree[Int] = {
    def loop(n: Int, acc: Tree[Int]): Tree[Int] =
      if (n <= 0)
        acc
      else
        loop(n - 1, Node(acc, Leaf(0)))

    loop(50000, Leaf(1))
  }
}