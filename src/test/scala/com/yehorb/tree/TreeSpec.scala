package com.yehorb.tree

import org.scalatest.{FlatSpec, Matchers}

trait IntStuff {
  implicit val zero: Int = 0

  val lessThan: (Int, Int) => Boolean = (a, b) => a < b
  val moreThan: (Int, Int) => Boolean = (a, b) => a > b

  val lessEqualsThan: (Int, Int) => Boolean = (a, b) => a <= b
  val moreEqualsThan: (Int, Int) => Boolean = (a, b) => a >= b

  val add: (Int, Int) => Int = (a, b) => a + b
}

trait FunStuff {
  val alwaysTrue: (Nothing, Nothing) => Boolean = (_, _) => true
  val alwaysFalse: (Nothing, Nothing) => Boolean = (_, _) => false
}

class TreeSpec extends FlatSpec with Matchers with IntStuff with FunStuff {
  "Node companion object" should "properly make new tree from individual pieces" in {
    Node(Leaf(1), Leaf(2), Node(Leaf(3), Leaf(4)), Leaf(5)) shouldEqual new Node(List(Leaf(1), Leaf(2), Node(List(Leaf(3), Leaf(4))), Leaf(5)))
  }

  "Node companion object" should "properly make new tree from List" in {
    Node(List(Leaf(1), Leaf(2), Node(List(Leaf(3), Leaf(4))), Leaf(5))) shouldEqual new Node(List(Leaf(1), Leaf(2), Node(List(Leaf(3), Leaf(4))), Leaf(5)))
  }

  "Node" should "rearrange children so nodes first, leafs last and sort leafs" in {
    val testNode = Node(Leaf(5), Leaf(3), Node(Leaf(4), Leaf(1)), Leaf(6), Node[Int](Nil))
    testNode.normalize(lessThan) shouldEqual Node(Node[Int](Nil), Node(Leaf(4), Leaf(1)), Leaf(3), Leaf(5), Leaf(6))
    testNode.normalize(moreThan) shouldEqual Node(Node[Int](Nil), Node(Leaf(4), Leaf(1)), Leaf(6), Leaf(5), Leaf(3))
  }

  "Empty node" should "not change" in {
    val testNode = Node(Nil)
    testNode.normalize(alwaysTrue) shouldEqual Node(Nil)
  }

  "Node with leafs only" should "sort leafs" in {
    val testNode = Node(Leaf(2), Leaf(1), Leaf(3))
    testNode.normalize(lessThan) shouldEqual Node(Leaf(1), Leaf(2), Leaf(3))
    testNode.normalize(moreThan) shouldEqual Node(Leaf(3), Leaf(2), Leaf(1))
  }

  "Nde with nodes only" should "not change" in {
    val testNode = Node(Node(Nil), Node(Nil), Node(Nil))
    testNode.normalize(alwaysTrue) shouldEqual Node(Node(Nil), Node(Nil), Node(Nil))
  }
}
