package Huffman

import scala.collection.immutable.SortedSet
import zio.Chunk

enum BinaryTree[+A] {
  case Leaf(a: A)
  case Branch(zero: BinaryTree[A], one: BinaryTree[A])

  def getMap[B >: A]: Map[B, String] = getMap("")

  private def getMap[B >: A](traversal: String): Map[B, String] = this
    match
      case Leaf(c) => Map(c -> traversal)
      case Branch(zero, one) => zero.getMap(traversal + "0") ++ one.getMap[B](traversal + "1")

  def get(char: Char) = this
    match
      case Leaf(_) => this
      case Branch(a, b) => 
        if char == '1' then 
          b
        else 
          a
          
  def isLeaf: Boolean = this
    match
      case Leaf => true
      case _ => false

}

object BinaryTree {

  def build[A](stats: Chunk[(A, Long)])(using ord: Ordering[A]): Option[BinaryTree[A]] = 
    val set = SortedSet[FrequencyTree[A]](stats.map(p => FrequencyTree.Leaf(p._1, p._2)): _*)(Ordering.by[FrequencyTree[A],Long](_.count).orElseBy(_.element))
    FrequencyTree.build(set).map(_.toBinaryTree)
  
  def read(str: String): Either[Throwable, BinaryTree[Byte]] = ???

  def write(tree: BinaryTree[Byte]): String = ???
}