package Huffman

import scala.collection.immutable.SortedSet

enum FrequencyTree[+A] {

  case Leaf(a: A, cnt: Long)
  case Branch(a: A, cnt: Long, zero: FrequencyTree[A], one: FrequencyTree[A])

  def count: Long = this
    match
      case Leaf(_, c) => c
      case Branch(_, c, _, _) => c
  
  def element: A = this 
    match
      case Leaf(a, _) => a
      case Branch(a, _, _, _) => a

  def merge[B >: A](other: FrequencyTree[B]): FrequencyTree[B] = Branch(this.element, this.count + other.count, this, other)

  def toBinaryTree: BinaryTree[A] = this 
    match
      case Leaf(a, _) => BinaryTree.Leaf(a)
      case Branch(_, _, one, zero) => BinaryTree.Branch(one.toBinaryTree, zero.toBinaryTree)

}

object FrequencyTree {

  def build[A](sortedSet: SortedSet[FrequencyTree[A]]): Option[FrequencyTree[A]] = 
    if sortedSet.isEmpty then
      None
    else if sortedSet.size == 1 then
      Some(sortedSet.head)
    else
      val (two, rest) = sortedSet.splitAt(2)
      build(rest + two.reduce(_.merge(_)))

}