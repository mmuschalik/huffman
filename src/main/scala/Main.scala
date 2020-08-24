import zio.stream._
import zio._
import zio.console._
import zio.blocking.Blocking
import java.io.{FileInputStream,FileOutputStream}
import java.nio.file.Path;
import java.nio.file.Paths;
import java.io.File

object MyApp extends App {

  def run(args: List[String]) =
    exec(args).fold(x => {println(x); ExitCode.failure}, _ => ExitCode.success)

  def exec(args: List[String]) = 
    for
      config <- ZIO.fromOption(Cli.parse(args))
      f       = if config.compress then
                  compress
                  else
                  decompress
      _      <- f(config.in.toPath, config.out.toPath)
    yield ()

  def compress(input: Path, output: Path) = 
    Huffman.buildTreeFromFile(input)
      .flatMap(tree => fileStream(input, output, Huffman.compress(tree)))

  def fileStream(input: Path, output: Path, transform: ZStream[Blocking, Throwable, Byte] => ZStream[Blocking, Throwable, Byte]) =
    transform(ZStream.fromFile(input)).run(ZSink.fromFile(output))

  def decompress(input: Path, output: Path) = 
    Huffman.decompress(ZStream.fromFile(input)).use {
      case stream => stream.run(ZSink.fromFile(output))
    }
}
