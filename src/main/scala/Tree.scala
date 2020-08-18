import scala.collection.immutable.SortedSet
import scala.util.parsing.combinator._

enum Tree {

  case Leaf(char: Char, cnt: Long)
  case Branch(zero: Tree, one: Tree, cnt: Long)

  def count: Long = this
    match
      case Leaf(_, c) => c
      case Branch(_, _, c) => c
  

  def name: String = this 
    match
      case Leaf(c, _) => c.toString
      case Branch(a, b, _) => a.name + b.name

  def getMap: Map[Char, String] = getMap("")

  private def getMap(traversal: String): Map[Char, String] = this
    match
      case Leaf(c, _) => Map(c -> traversal)
      case Branch(zero, one, _) => zero.getMap(traversal + "0") ++ one.getMap(traversal + "1")

  def merge(other: Tree): Tree =
    Branch(this, other, this.count + other.count)

  def get(char: Char) = this
    match
      case l: Leaf => this
      case Branch(a, b, _) => 
        if char == '1' then 
          b
        else 
          a
  
}

object Tree {

  def buildHuffmanTree(sortedSet: SortedSet[Tree]): Option[Tree] = 
    if sortedSet.isEmpty then
      None
    else if sortedSet.size == 1 then
      Some(sortedSet.head)
    else
      val (two, rest) = sortedSet.splitAt(2)
      buildHuffmanTree(rest + two.reduce(_.merge(_)))

  def show(tree: Tree): String = tree
    match
      case Leaf(c, _) => "'" + c.toString
      case Branch(zero, one, _) => "|" + show(zero) + show(one)

}

object TreeParser extends RegexParsers {

  val letter = regex(".".r)

  def leaf: Parser[Tree] =  literal("'") ~ letter ^^ { case _ ~ s => Tree.Leaf(s.head, 0) }
  def branch: Parser[Tree] = literal("|") ~ tree ~ tree ^^ { case _ ~ left ~ right => Tree.Branch(left, right, 0) }

  def tree: Parser[Tree] = leaf | branch

  def apply(str: String) = parse(tree, str)
    match
      case Success(t, _) => Right(t)
      case Failure(f, _) => Left(ParseException(f))
      case Error(e, _) => Left(ParseException(e))

}

case class ParseException(msg: String) extends Exception