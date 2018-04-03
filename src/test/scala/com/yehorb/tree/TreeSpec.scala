package com.yehorb.tree

import org.scalatest.{FlatSpec, Matchers}

trait IntStuff {
  implicit val zero: Int = 0

  val ascending: (Int, Int) => Boolean = (a, b) => a < b
  val descending: (Int, Int) => Boolean = (a, b) => a > b

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
    testNode.normalize(ascending) shouldEqual Node(Node[Int](Nil), Node(Leaf(4), Leaf(1)), Leaf(3), Leaf(5), Leaf(6))
    testNode.normalize(descending) shouldEqual Node(Node[Int](Nil), Node(Leaf(4), Leaf(1)), Leaf(6), Leaf(5), Leaf(3))
  }

  "Empty node" should "not change after sorting" in {
    val testNode = Node(Nil)
    testNode.normalize(alwaysTrue) shouldEqual testNode
  }

  "Node with leafs only" should "sort leafs" in {
    val testNode = Node(Leaf(2), Leaf(1), Leaf(3))
    testNode.normalize(ascending) shouldEqual Node(Leaf(1), Leaf(2), Leaf(3))
    testNode.normalize(descending) shouldEqual Node(Leaf(3), Leaf(2), Leaf(1))
  }

  "Node with nodes only" should "not change after sorting" in {
    val testNode = Node(Node(Nil), Node(Nil), Node(Nil))
    testNode.normalize(alwaysTrue) shouldEqual testNode
  }

  "Node" should "properly sort children and separate leftovers" in {
    val testNode = Node(Leaf(2), Node[Int](Nil), Node(Leaf(1), Leaf(5)), Leaf(1), Leaf(6), Leaf(3), Node[Int](Nil))
    testNode.separate(6, ascending, lessEqualsThan, add) shouldEqual (Node(Node[Int](Nil), Node(Leaf(1), Leaf(5)), Node[Int](Nil), Leaf(1), Leaf(2), Leaf(3)), List(Leaf(6)))
    Node(Leaf(1)).separate(6, ascending, lessEqualsThan, add) shouldEqual (Node(Leaf(1)), Nil)
    Node(Leaf(1)).separate(6, ascending, moreEqualsThan, add) shouldEqual (Node(Nil), List(Leaf(1)))
  }

  "Empty node" should "not change after separation" in {
    val testNode = Node[Int](Nil)
    testNode.separate(0, ascending, lessEqualsThan, add) shouldEqual (testNode, Nil)
  }

  "Node with leafs only" should "properly sort leafs and separate leftovers" in {
    val testNode = Node(Leaf(2), Leaf(1), Leaf(3), Leaf(5), Leaf(4))
    testNode.separate(6, ascending, lessEqualsThan, add) shouldEqual (Node(Leaf(1), Leaf(2), Leaf(3)), List(Leaf(4), Leaf(5)))
    testNode.separate(9, descending, lessEqualsThan, add) shouldEqual (Node(Leaf(5), Leaf(4)), List(Leaf(3), Leaf(2), Leaf(1)))
  }

  "Node with nodes only" should "not change after separation" in {
    val testNode = Node(Node[Int](Nil), Node[Int](Nil), Node[Int](Nil))
    testNode.separate(0, ascending, lessEqualsThan, add) shouldEqual (testNode, Nil)
  }

  "Tree" should "sort out properly on default data" in {
    //                  a1              b2       a2              b3       a3              b4       b1
    val testNode = Node(Node[Int](Nil), Leaf(2), Node[Int](Nil), Leaf(3), Node[Int](Nil), Leaf(4), Leaf(1))
    // Sorted ascending and tested as sum <= w where w = 3. There are no b4 leaf in sorted a1 because it was sorted out, as 3 + 4 is not <= 3. Only 3 is.
    //                                                                a3              a2              a1(b3)         b1       b2
    testNode.sort(3, ascending, lessEqualsThan, add) shouldEqual Node(Node[Int](Nil), Node[Int](Nil), Node(Leaf(3)), Leaf(1), Leaf(2))
    // Similar case, but now w = 3. Notice than b1, b2, b3 present in parent node as 1 + 2 + 3 <= 6 and b4 is present in first child node as 4 <= 6 too.
    //                                                                a3              a2              a1(b4)         b1       b2       b3
    testNode.sort(6, ascending, lessEqualsThan, add) shouldEqual Node(Node[Int](Nil), Node[Int](Nil), Node(Leaf(4)), Leaf(1), Leaf(2), Leaf(3))
  }
}
