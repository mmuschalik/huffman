import zio.stream._
import zio._
import zio.console._
import zio.blocking.Blocking
import java.io.{FileInputStream,FileOutputStream}
import Tree._
import java.io.{File,IOException}
import scala.collection.immutable.SortedSet

def getContentStream(inputStream: FileInputStream, tree: Tree) =
  val treeString = Tree.show(tree)
  val headerSize =  Stream.fromIterable(BigInt(treeString.length).toByteArray.reverse.padTo(4, 0x00.toByte).reverse)
  val header = Stream.fromIterable(treeString.getBytes)
  val body = compressStream(ZStream.fromInputStream(inputStream).map(_.toChar), tree.getMap)

  headerSize ++ header ++ body

def getTreeStream[R, E](stream: ZStream[R , E, Byte]) =
  for
    headerSize <- stream.peel(ZSink.take(4))
    header     <- headerSize._2.peel(ZSink.take(BigInt(headerSize._1.toArray).toInt))
    treeEither  = TreeParser(header._1.map(_.toChar).mkString)
    treeStream  = treeEither.fold(f => Stream.fail(f), t => unCompressStream(header._2, t))
  yield treeStream

def buildTreeFromFile(inputFile: ZManaged[Any, Throwable, FileInputStream]) =
  for
    treeOption <- inputFile.use { case inputStream => getStats(ZStream.fromInputStream(inputStream).map(_.toChar)).map(stats => statsToTree(stats)) }
    tree       <- ZIO.fromOption(treeOption)
    totalBytes  = tree.count
    _          <- putStrLn("Total bytes read: " + totalBytes.toString)
    _          <- putStrLn(tree.getMap.toString)
  yield (tree)

def getStats[R, E, O](stream: ZStream[R, E, O]) =
  stream.groupByKey(c => c) { case (k, s) => ZStream.fromEffect(s.runCount.map(c => (k, c))) }.runCollect

def statsToSeed(stats: Chunk[(Char,Long)]): SortedSet[Tree] =
  SortedSet[Tree](stats.map(p => Leaf(p._1, p._2)): _*)(Ordering.by[Tree,Long](_.count).orElseBy(_.name))

def statsToTree(stats: Chunk[(Char,Long)]): Option[Tree] =
  buildHuffmanTree(statsToSeed(stats))

def compressStream[R](incoming: ZStream[R, IOException, Char], myLookup: Map[Char, String]): ZStream[R, IOException, Byte] =
  incoming
    .flatMap(c => myLookup.get(c).map(Stream.fromIterable).getOrElse(Stream.fail(IOException("Incompatible Huffman Tree"))))
    .grouped(8)
    .map(bitStringToByte)

def unCompressStream[R, E](incoming: ZStream[R, E, Byte], tree: Tree): ZStream[R, E, Tree] =
  incoming
    .flatMap(b => Stream.fromIterable(byteToBitString(b)))
    .aggregate(ZTransducer.fold(tree)(t => !t.isInstanceOf[Leaf])((t,b) => t.get(b)))

def bitStringToByte(str: Chunk[Char]): Byte = 
  Integer.parseInt(str.mkString.padTo(8, '0'), 2).toByte

def byteToBitString(byte: Byte): String = 
  String.format("%8s", Integer.toBinaryString(byte & 0xFF)).replace(' ', '0')

//def contentLengthStream(long: Long): ZStream[BlockingIOException, Byte] = 
//  Stream.fromIterable(BigInt(long).toByteArray.reverse.padTo(8, 0x00.toByte).reverse)