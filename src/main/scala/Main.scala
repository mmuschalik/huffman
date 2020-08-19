import zio.stream._
import zio._
import zio.console._
import zio.blocking.Blocking
import java.io.{FileInputStream,FileOutputStream}
import java.nio.file.Path;
import java.nio.file.Paths;

object MyApp extends App {

  def run(args: List[String]) =
    //compress.fold(x => ExitCode.failure, _ => ExitCode.success)
    decompress.fold(x => ExitCode.failure, _ => ExitCode.success)

  def compress = {

    val inputFile = Managed.make(Task(new FileInputStream("sample.txt")))(os => UIO(os.close()))
    val outputFile = Managed.make(Task(new FileOutputStream("compressed.dat")))(os => UIO(os.close()))

    val resources =
      for
        i <- inputFile
        o <- outputFile
      yield (i, o)

    for
      tree <- buildTreeFromFile(inputFile)
      _    <- resources.use { case (inputStream, outputStream) => 
                getContentStream(inputStream, tree).run(ZSink.fromOutputStream(outputStream)) }
    yield ()
  }

  def decompress = getTreeStream(ZStream.fromFile(Paths.get("compressed.dat"))).use { 
    case stream => stream.run(ZSink.fromFile(Paths.get("sample2.txt")))
  }
  
}