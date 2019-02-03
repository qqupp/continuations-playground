package example

import org.scalatest.{FlatSpec, Matchers}
import data.Tree.Tree._
import data.Tree.Tree

class TreeTests extends FlatSpec with Matchers with TestContext {

  "MapTreeNaiveTest" should "map a deep tree with stack overflow" in {
    assertThrows[StackOverflowError](MapTreeNaive.map(deepIntTree)(_.toString))
  }


  "MapTreeContinuation" should "map a deep tree with stack overflow" in {
    assertThrows[StackOverflowError](MapTreeContinuation.map(deepIntTree)(_.toString))
  }

  "MapTreeContinuationTailrec" should "map a deep tree with no problems" in {
    MapTreeContinuationTailrec.map(deepIntTree)(_.toString) shouldBe a[Tree[Int]]
  }


  "MapTreeContinuationMonad" should "map a deep tree with stack overflow" in {
    assertThrows[StackOverflowError](MapTreeContinuationMonad.map(deepIntTree)(_.toString))
  }

}
trait TestContext {

  val deepIntTree = deepTree(1000000)
}
