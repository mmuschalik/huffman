import zio.stream._
import zio._
import zio.console._
import zio.blocking.Blocking
import java.io.{FileInputStream,FileOutputStream}

object MyApp extends App {

  def run(args: List[String]) =
    myApp.fold(x => ExitCode.failure, _ => ExitCode.success)

  def myApp = {

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
}