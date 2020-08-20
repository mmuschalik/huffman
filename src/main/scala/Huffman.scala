import zio.stream._
import zio._
import zio.console._
import zio.blocking.Blocking
import java.io.{FileInputStream,FileOutputStream}
import Tree._
import java.io.{File,IOException}
import scala.collection.immutable.SortedSet

def getTreeStream[R, E](stream: ZStream[R , E, Byte]) =
  for
    headerSize <- stream.peel(ZSink.take(4))
    header     <- headerSize._2.peel(ZSink.take(BigInt(headerSize._1.toArray).toInt))
    treeEither  = TreeParser(header._1.map(_.toChar).mkString)
    treeStream  = treeEither.fold(f => Stream.fail(f), t => unCompressStream(header._2, t))
  yield treeStream

def unCompressStream[R, E](incoming: ZStream[R, E, Byte], tree: Tree): ZStream[R, E, Byte] =
  incoming
    .flatMap(b => Stream.fromIterable(byteToBitString(b)))
    .aggregate(ZTransducer.fold(tree)(t => !t.isInstanceOf[Leaf])((t,b) => t.get(b)))
    .map(_.name.head.toByte)

def byteToBitString(byte: Byte): String = 
  String.format("%8s", Integer.toBinaryString(byte & 0xFF)).replace(' ', '0')

//def contentLengthStream(long: Long): ZStream[BlockingIOException, Byte] = 
//  Stream.fromIterable(BigInt(long).toByteArray.reverse.padTo(8, 0x00.toByte).reverse)