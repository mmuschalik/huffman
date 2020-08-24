package Cli

import scopt.OParser
import java.io.File
import builder._

case class Config(
  in: File = new File("."),
  out: File = new File("."),
  compress: Boolean = true)


val builder = OParser.builder[Config]

val parser1 =
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

def parse(args: List[String]) = OParser.parse(parser1, args, Config()) 