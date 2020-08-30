package Huffman

import scala.collection.immutable.SortedSet
import zio.Chunk
import zio.stream._
import zio._
import java.nio.file.Path;
import java.io.{File,IOException}

type Cell = Option[Byte]

def buildTreeFromFile(path: Path): ZIO[blocking.Blocking, Throwable, BinaryTree[Cell]] =
  getStats(ZStream.fromFile(path).map(Some(_)) ++ Stream(None))
    .flatMap(stats => ZIO.fromOption(BinaryTree.build(stats))
    .mapError(_ => Exception("Empty file?")))

def getStats[R, E, O](stream: ZStream[R, E, O]): ZIO[R, E, Chunk[(O, Long)]] =
  stream
    .groupByKey(c => c) { case (k, s) => ZStream.fromEffect(s.runCount.map(c => (k, c))) }
    .runCollect
  
def compress[R](tree: BinaryTree[Cell]): ZStream[R, Throwable, Byte] => ZStream[R, Throwable, Byte] = stream =>
  val treeString = BinaryTree.write(tree)
  val headerSize =  Stream.fromIterable(BigInt(treeString.length).toByteArray.reverse.padTo(4, 0x00.toByte).reverse)
  val header = Stream.fromIterable(treeString.getBytes)
  val body = compressStream(tree.getMap)(stream)

  headerSize ++ header ++ body

def compressStream[R](myLookup: Map[Cell, String]): ZStream[R, Throwable, Byte] => ZStream[R, Throwable, Byte] = incoming =>
  incoming.map(Some(_))
    .concat(Stream(None))
    .flatMap(c => myLookup.get(c).map(Stream.fromIterable).getOrElse(Stream.fail(Exception("Incompatible Huffman Tree"))))
    .grouped(8)
    .map(bitStringToByte)

def decompress[R, E](tree: BinaryTree[Cell]): ZStream[R, E, Byte] => ZStream[R, E, Byte] = incoming =>
  incoming
    .flatMap(b => Stream.fromIterable(byteToBitString(b)))
    .aggregate(ZTransducer.fold(tree)(t => !t.isLeaf)((t,b) => t.get(b)))
    .takeUntil(l => 
      l match
        case BinaryTree.Leaf(None) => true
        case _ => false
      )
    .flatMap(l => 
      l match
        case BinaryTree.Leaf(Some(b)) => Stream(b)
        case _ => Stream())

def decompress[R](stream: ZStream[R , Throwable, Byte]): ZManaged[R, Throwable, ZStream[R , Throwable, Byte]] =
  for
    headerSize <- stream.peel(ZSink.take(4))
    header     <- headerSize._2.peel(ZSink.take(BigInt(headerSize._1.toArray).toInt))
    treeEither  = BinaryTree.read(header._1.map(_.toChar).mkString)
    outStream   = treeEither.fold(f => Stream.fail(f), t => decompress(t)(header._2))
  yield outStream


def bitStringToByte(str: Chunk[Char]): Byte = 
  Integer.parseInt(str.mkString.padTo(8, '0'), 2).toByte

def byteToBitString(byte: Byte): String = 
  String.format("%8s", Integer.toBinaryString(byte & 0xFF)).replace(' ', '0')