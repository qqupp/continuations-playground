package example

import org.scalatest.{FlatSpec, Matchers}
import data.Tree._

class TreeTests extends FlatSpec with Matchers with TestContext {

  "MapTreeNaiveTest" should "map a deep tree with stack overflow" in {
    assertThrows[StackOverflowError](MapTreeNaive.map(treeLeft)(_.toString))
    assertThrows[StackOverflowError](MapTreeNaive.map(treeRight)(_.toString))
  }

  "MapTreeContinuation" should "map a deep tree with stack overflow" in {
    assertThrows[StackOverflowError](MapTreeContinuation.map(treeLeft)(_.toString))
    assertThrows[StackOverflowError](MapTreeContinuation.map(treeRight)(_.toString))
  }

  "MapTreeContinuationMonad" should "map a deep tree with stack overflow" in {
    assertThrows[StackOverflowError](MapTreeContinuationMonad.map(treeLeft)(_.toString))
    assertThrows[StackOverflowError](MapTreeContinuationMonad.map(treeRight)(_.toString))
  }

  "MapTreeContinuationTailrec" should "map a deep tree with no problems" in {
    MapTreeContinuationTailrec.map(treeLeft)(_.toString) shouldBe a[Tree[_]]
    MapTreeContinuationTailrec.map(treeRight)(_.toString) shouldBe a[Tree[_]]
  }

  "MapTreeContinuationMonadTailrec" should "map a deep tree with some problems" in {
    MapTreeContinuationTailrecMonad.map(treeRight)(_.toString) shouldBe a[Tree[_]]
    assertThrows[StackOverflowError](MapTreeContinuationTailrecMonad.map(treeLeft)(_.toString))
  }

  "Comparing 2 identical deep trees with '==' " should " throws " in {
    assertThrows[StackOverflowError](tree1 == tree2)
  }

}

trait TestContext {

  private def loop(n: Int, acc: Tree[Int], f: Tree[Int] => Tree[Int]): Tree[Int] =
    if (n <= 0) acc
    else loop(n - 1, f(acc), f)

  private def deepLeftTree(height: Int): Tree[Int] =
    loop(height, Leaf(1), Node(_, Leaf(0)))

  private def deepRightTree(height: Int): Tree[Int] =
    loop(height, Leaf(1), Node(Leaf(0), _))


  val treeLeft = deepLeftTree(1000000)
  val treeRight = deepRightTree(1000000)


  val tree1 = deepLeftTree(10000)
  val tree2 = deepLeftTree(10000)

}
