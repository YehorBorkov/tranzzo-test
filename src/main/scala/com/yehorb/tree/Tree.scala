package com.yehorb.tree

/**
  * Base trait for tree components
  * @tparam A Type parameter of the tree. E.g. it can be tree of ints, strings, etc.
  */
sealed trait Tree[A]

/**
  * Tree's leaf component, used for storing values.
  * Only holds a value and has no special traits.
  * @param value Value of type A to be stored.
  * @tparam A    Type parameter of the leaf.
  */
case class Leaf[A](value: A) extends Tree[A]

/**
  * Tree's node component, used to define tree structure.
  * Can hold other nodes and leafs in its children list.
  * Be aware, does NOT store a value of type A.
  * @param children Children list of the node.
  * @tparam A       Type parameter of the node.
  */
case class Node[A](children: List[Tree[A]]) extends Tree[A]
object Node {
  /**
    * Construct new node from List[ Tree[A] ] or the varargs of type Tree[A]
    * @param  children List of children to be stored, or varargs
    * @tparam A        Type parameter of created node
    * @return          New node with defined children of type Node[A]
    */
  def apply[A](children: Tree[A]*): Node[A] = new Node(children.toList)
  def apply[A](children: List[Tree[A]]): Node[A] = new Node(children)
}
