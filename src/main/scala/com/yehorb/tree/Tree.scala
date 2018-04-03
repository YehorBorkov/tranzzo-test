package com.yehorb.tree

sealed trait Tree[A]

case class Leaf[A](value: A) extends Tree[A]

case class Node[A](trees: List[Tree[A]]) extends Tree[A]
object Node {
  def apply[A](trees: Tree[A]*): Node[A] = new Node(trees.toList)
  def apply[A](trees: List[Tree[A]]): Node[A] = new Node(trees)
}
