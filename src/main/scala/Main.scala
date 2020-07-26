import zio.stream._
import zio._
import Tree._
import scala.collection.immutable.SortedSet
object Main {

  def main(args: Array[String]): Unit = {

    val myText = Stream.fromIterable("A_DEAD_DAD_CEDED_A_BAD_BABE_A__".toArray)

    val sortedSet = SortedSet[Tree](Leaf('C', 2), Leaf('B', 6), Leaf('E', 7), Leaf('_', 10), Leaf('D', 10), Leaf('A', 11))(Ordering.by[Tree,Int](_.count).orElseBy(_.name))
  
    val tree = buildHuffmanTree(sortedSet).get


    val myLookup: Map[Char, String] = tree.getMap
    
    // maybe flatmap and add the offset as final char
    // when decrypting, take at least 8 chars, then work out what to drop

    val compressed = compressStream(myText, myLookup)

    val uncompressed = unCompressStream(compressed, tree)
    
    val runtime = Runtime.default

    runtime.unsafeRun(
      uncompressed.foreach(r => Task(println(r)))
    )

    

  }

}

def compressStream(incoming: Stream[Nothing, Char], myLookup: Map[Char, String]): Stream[String, Byte] =
  incoming
    .flatMap(c => myLookup.get(c).map(Stream.fromIterable).getOrElse(Stream.fail("Incompatible Huffman Tree")))
    .grouped(8)
    .map(bitStringToByte)

def unCompressStream(incoming: Stream[String, Byte], tree: Tree): Stream[String, Tree] =
  incoming
    .flatMap(b => Stream.fromIterable(byteToBitString(b)))
    .aggregate(ZTransducer.fold(tree)(t => !t.isInstanceOf[Leaf])((t,b) => t.get(b)))

def bitStringToByte(str: List[Char]): Byte = 
  Integer.parseInt(str.mkString.padTo(8, '0'), 2).toByte

def byteToBitString(byte: Byte): String = 
  String.format("%8s", Integer.toBinaryString(byte & 0xFF)).replace(' ', '0')