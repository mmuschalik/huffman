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
    import scopt.OParser

    val builder = OParser.builder[Config]
    val parser1 = {
      import builder._
      OParser.sequence(
        programName("huff"),
        head("huff", "0.1"),
        opt[File]('i', "in")
        .required()
        .valueName("<file>")
        .action((x, c) => c.copy(in = x))
        .text("in is a required file property"),
        opt[File]('o', "out")
          .required()
          .valueName("<file>")
          .action((x, c) => c.copy(out = x))
          .text("out is a required file property"),
        opt[Unit]('d', "decompress")
          .action((_, c) => c.copy(compress = false))
          .text("decompress instead of compress"))
    }

    val r = for
      config <- ZIO.fromOption(OParser.parse(parser1, args, Config()))
      f       = if config.compress then
                  compress
                  else
                  decompress
      _      <- f(config.in.toPath, config.out.toPath)
    yield ()

    r.fold(x => {println(x); ExitCode.failure}, _ => ExitCode.success)

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

case class Config(
  in: File = new File("."),
  out: File = new File("."),
  compress: Boolean = true)