import zio.stream._
import zio._
import zio.console._
import zio.blocking.Blocking
import java.io.{FileInputStream,FileOutputStream}

object MyApp extends App {

  def run(args: List[String]) =
    myApp.fold(x => ExitCode.failure, _ => ExitCode.success)

  def myApp = {

    val inputFile = Managed.make(IO.effect(new FileInputStream("sample.txt")))(os => IO.effectTotal(os.close()))
    val outputFile = Managed.make(IO.effect(new FileOutputStream("compressed.dat")))(os => IO.effectTotal(os.close()))

    val resources =
      for
        i <- inputFile
        o <- outputFile
      yield (i, o)

    val treeIO = buildTreeFromFile(inputFile)

    treeIO.flatMap(tree =>
      resources.use { case (inputStream, outputStream) =>
        getContentStream(inputStream, tree).run(ZSink.fromOutputStream(outputStream)) 
      }
    )
  }
}