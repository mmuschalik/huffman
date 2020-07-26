import zio.stream._
import zio._
import zio.console._
import Tree._
import scala.collection.immutable.SortedSet
import java.io.FileOutputStream
import java.io.File

object MyApp extends App {

  def run(args: List[String]) =
    myApp.fold(x => ExitCode.failure, _ => ExitCode.success)

  def myApp = {

    val myText = Stream.fromIterable("A_DEAD_DAD_CEDED_A_BAD_BABE_A__".toArray)

    val os = new FileOutputStream(new File("test.dat"))

    for
      stats      <- getStats(myText)
      tree       <- ZIO.fromOption(statsToTree(stats))
      totalBytes  = tree.count
      _          <- putStrLn("Total bytes read: " + totalBytes.toString)
      _          <- putStrLn(tree.getMap.toString)
      compress    = compressStream(myText, tree.getMap)
      content     = contentLengthStream(totalBytes) ++ compress
      _          <- content.run(ZSink.fromOutputStream(os))
    yield ()
  }
    
}

def getStats(stream: Stream[Nothing, Char]) =
  stream.groupByKey(c => c) { case (k, s) => ZStream.fromEffect(s.runCount.map(c => (k, c))) }.runCollect

def statsToSeed(stats: Chunk[(Char,Long)]): SortedSet[Tree] =
  SortedSet[Tree](stats.map(p => Leaf(p._1, p._2)): _*)(Ordering.by[Tree,Long](_.count).orElseBy(_.name))

def statsToTree(stats: Chunk[(Char,Long)]): Option[Tree] =
  buildHuffmanTree(statsToSeed(stats))

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

def contentLengthStream(long: Long): Stream[Nothing, Byte] = 
  Stream.fromIterable(BigInt(long).toByteArray.reverse.padTo(8, 0x00.toByte).reverse)