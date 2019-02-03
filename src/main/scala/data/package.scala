package object Tree {

  sealed trait Tree[T] extends Product with Serializable
  final case class Node[T](left: Tree[T], right: Tree[T]) extends Tree[T]
  final case class Leaf[T](value: T) extends Tree[T]

  def deepTree(height: Int): Tree[Int] = {
    def loop(n: Int, acc: Tree[Int]): Tree[Int] =
      if (n <= 0)
        acc
      else
        loop(n - 1, Node(acc, Leaf(0)))

    loop(height, Leaf(1))
  }

}
