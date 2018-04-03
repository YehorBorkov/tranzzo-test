package com.yehorb.tree

import org.scalatest.{FlatSpec, Matchers}

class TreeSpec extends FlatSpec with Matchers {
  "Node companion object" should "properly make new tree from individual pieces" in {
    Node(Leaf(1), Leaf(2), Node(Leaf(3), Leaf(4)), Leaf(5)) shouldEqual new Node(List(Leaf(1), Leaf(2), Node(List(Leaf(3), Leaf(4))), Leaf(5)))
  }

  "Node companion object" should "properly make new tree from List" in {
    Node(List(Leaf(1), Leaf(2), Node(List(Leaf(3), Leaf(4))), Leaf(5))) shouldEqual new Node(List(Leaf(1), Leaf(2), Node(List(Leaf(3), Leaf(4))), Leaf(5)))
  }
}
