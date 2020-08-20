import zio.stream._
import zio._
import zio.console._
import zio.blocking.Blocking
import java.io.{FileInputStream,FileOutputStream}
import java.nio.file.Path;
import java.nio.file.Paths;

object MyApp extends App {

  def run(args: List[String]) =
    compress(Paths.get("sample.txt"), Paths.get("compressed.dat")).fold(x => ExitCode.failure, _ => ExitCode.success)

  def compress(input: Path, output: Path) = 
    Huffman.buildTreeFromFile(input)
      .flatMap(tree => Huffman.compress(tree)(ZStream.fromFile(input)).run(ZSink.fromFile(output)))

  //def decompress(input: Path, output: Path) = 
  //  getTreeStream(ZStream.fromFile(input)).use { 
  //    case stream => stream.run(ZSink.fromFile(output))
  //  }
  
}