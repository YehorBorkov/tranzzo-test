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
case class Node[A](children: List[Tree[A]]) extends Tree[A] {
  /**
    * Splits children to nodes and leafs and sorts leafs of given node using provided comparator function
    * @param sort Comparator function for comparing arguments of type A
    * @return     New node with nodes and leafs separated, leafs sorted
    */
  def normalize(sort: (A, A) => Boolean): Node[A] = {
    val (nodes, leafs) = _splitSort(sort)
    this.copy(nodes ++ leafs)
  }

  // Private node logic for manipulating children list
  private def _split: (List[Node[A]], List[Leaf[A]]) = {
    def loop(list: List[Tree[A]], nodes: List[Node[A]], leafs: List[Leaf[A]]): (List[Node[A]], List[Leaf[A]]) = {
      list match {
        case s :: xs => s match {
          case n: Node[A] => loop(xs, n :: nodes, leafs)
          case l: Leaf[A] => loop(xs, nodes, l :: leafs)
        }
        case Nil => (nodes, leafs)
      }
    }

    loop(children, Nil, Nil)
  }

  private def _sort(list: List[Leaf[A]], how: (A, A) => Boolean): List[Leaf[A]] = {
    def merge(left: List[Leaf[A]], right: List[Leaf[A]]): List[Leaf[A]] =
      (left, right) match {
        case (_, Nil) => left
        case (Nil, _) => right
        case (leftHead :: leftTail, rightHead :: rightTail) =>
          if (how(leftHead.value, rightHead.value))
            leftHead :: merge(leftTail, right)
          else
            rightHead :: merge(left, rightTail)
      }

    def sort(list: List[Leaf[A]]): List[Leaf[A]] = {
      list.length / 2 match {
        case 0 => list
        case n =>
          val (left, right) = list.splitAt(n)
          merge(sort(left), sort(right))
      }
    }

    sort(list)
  }

  private def _splitSort(how: (A, A) => Boolean): (List[Node[A]], List[Leaf[A]]) = {
    val (nodes, leafs) = _split
    (nodes, _sort(leafs, how))
  }
}
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
