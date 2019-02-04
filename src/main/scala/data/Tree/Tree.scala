package data.Tree

sealed trait Tree[T] extends Product with Serializable

final case class Leaf[T](value: T) extends Tree[T]
final case class Node[T](left: Tree[T], right: Tree[T]) extends Tree[T]
