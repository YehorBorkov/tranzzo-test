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
    * Sorts the whole tree recursively by given rules:
    * Leafs are sorted according to given '''sort''' function.
    * Than leafs are combined (aka foldLeft) using '''combine''' function.
    * Combination goes on until '''compare''' against '''w''' is true.
    * Remaining leafs are considered leftovers and moved to the firs-in-list child node.
    * Leftovers on the bottom nodes are discarded.
    * Sorting goes on recursively from top to bottom.
    * @param w       Nominal value combination is tested against
    * @param sort    Sorter function, how to sort leafs
    * @param compare Comparator function, how to compare A to w (w comes last e.g. it's sum < w, not w < sum)
    * @param combine Combiner function, how to combine A's (sum comes first e.g. it's sum - a, not a - sum)
    * @param init    Initial (zeroed) value of A
    * @return        Recursively sorted tree from top to bottom
    */
  def sort(w: A, sort: (A, A) => Boolean, compare: (A, A) => Boolean, combine: (A, A) => A)(implicit init: A): Node[A] = {
    def mainLoop(root: Node[A], bonus: List[Leaf[A]]): Node[A] = {
      val (nodes, carry, newBonus) = root._disassemble(w, sort, compare, combine, bonus)

      def childLoop(nodes: List[Node[A]], fromParent: List[Leaf[A]], acc: List[Node[A]]): List[Node[A]] = {
        nodes match {
          case s :: xs =>
            fromParent match {
              case Nil  => childLoop(xs, Nil, mainLoop(s, Nil) :: acc)
              case data => childLoop(xs, Nil, mainLoop(s, data) :: acc)
            }
          case Nil =>
            acc
        }
      }

      new Node[A](childLoop(nodes, newBonus, Nil) ++ carry)
    }

    mainLoop(this, Nil)
  }

  /**
    * Normalizes the node and filters combination of leafs against given w using how
    * @param w       Nominal value combination is tested against
    * @param sort    Sorter function, how to sort leafs
    * @param compare Comparator function, how to compare A to w (w comes last e.g. it's sum < w, not w < sum)
    * @param combine Combiner function, how to combine A's (sum comes first e.g. it's sum - a, not a - sum)
    * @param init    Initial (zeroed) value of A
    * @return        Normalized node with leftovers in tuple
    */
  def separate(w: A, sort: (A, A) => Boolean, compare: (A, A) => Boolean, combine: (A, A) => A)(implicit init: A): (Node[A], List[Leaf[A]]) = {
    val (nodes, leafs) = _splitSort(sort)
    val (carry, leftovers) = _filterOut(leafs, w, sort, compare, combine)

    (this.copy(nodes ++ carry), leftovers)
  }

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

  private def _filterOut(list: List[Leaf[A]], w: A, sort: (A, A) => Boolean, compare: (A, A) => Boolean, combine: (A, A) => A)(implicit init: A): (List[Leaf[A]], List[Leaf[A]]) = {
    def loop(sum: A, list: List[Leaf[A]], valid: List[Leaf[A]], invalid: List[Leaf[A]]): (List[Leaf[A]], List[Leaf[A]]) = {
      list match {
        case s :: xs =>
          val cmb = combine(sum, s.value)
          if (compare(cmb, w))
            loop(cmb, xs, valid :+ s, invalid)
          else
            loop(cmb, xs, valid, invalid :+ s)
        case Nil =>
          (valid, invalid)
      }
    }

    loop(init, list, Nil, Nil)
  }

  private def _disassemble(w: A, sort: (A, A) => Boolean, compare: (A, A) => Boolean, combine: (A, A) => A, bonus: List[Leaf[A]])(implicit init: A): (List[Node[A]], List[Leaf[A]], List[Leaf[A]]) = {
    val (nodes, leafs) = _splitSort(sort)
    val (carry, leftovers) = _filterOut(leafs ++ bonus, w, sort, compare, combine)

    (nodes, carry, leftovers)
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
