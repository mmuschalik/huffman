package Huffman

import scala.collection.immutable.SortedSet
import zio.Chunk
import scala.util.parsing.combinator._

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
      case Leaf(_) => true
      case _ => false

}

object BinaryTree {

  def build[A](stats: Chunk[(A, Long)])(using ord: Ordering[A]): Option[BinaryTree[A]] = 
    val set = SortedSet[FrequencyTree[A]](stats.map(p => FrequencyTree.Leaf(p._1, p._2)): _*)(Ordering.by[FrequencyTree[A],Long](_.count).orElseBy(_.element))
    FrequencyTree.build(set).map(_.toBinaryTree)
  
  def read(str: String): Either[Throwable, BinaryTree[Cell]] = BinaryTreeParser(str)

  def write(tree: BinaryTree[Cell]): String = tree
    match
      case Leaf(Some(c)) => "'" + c.toChar.toString
      case Leaf(None) => "!"
      case Branch(zero, one) => "|" + write(zero) + write(one)

}


object BinaryTreeParser extends RegexParsers {

  val letter = regex(".".r)

  def leaf: Parser[BinaryTree[Cell]] =  literal("'") ~ letter ^^ { case _ ~ s => BinaryTree.Leaf(Some(s.head.toByte)) }
  def eof: Parser[BinaryTree[Cell]] =  literal("!") ^^ { case _ => BinaryTree.Leaf(None) }
  def branch: Parser[BinaryTree[Cell]] = literal("|") ~ tree ~ tree ^^ { case _ ~ left ~ right => BinaryTree.Branch(left, right) }

  def tree: Parser[BinaryTree[Cell]] = eof | leaf | branch

  def apply(str: String) = parse(tree, str)
    match
      case Success(t, _) => Right(t)
      case Failure(f, _) => Left(Exception(f))
      case Error(e, _) => Left(Exception(e))

}
