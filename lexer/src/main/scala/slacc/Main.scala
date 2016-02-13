package slacc

import utils._
import java.io.File

import lexer._

object Main {

  def processOptions(args: Array[String]): Context = {

    val reporter = new Reporter()
    var ctx = Context(reporter = reporter)

    def processOption(args: List[String]): Unit = args match {
      case "--help" :: args =>
        ctx = ctx.copy(doHelp = true)
        processOption(args)

      case "--tokens" :: args =>
        ctx = ctx.copy(doTokens = true)
        processOption(args)

      case "-d" :: out :: args =>
        ctx = ctx.copy(outDir = Some(new File(out)))
        processOption(args)

      case f :: args =>
        ctx = ctx.copy(files = new File(f) :: ctx.files)
        processOption(args)

      case Nil =>
    }

    processOption(args.toList)

    if (ctx.doHelp) {
      displayHelp()
      sys.exit(0)
    }

    if (ctx.files.size != 1) {
      reporter.fatal("Exactly one file expected, "+ctx.files.size+" file(s) given.")
    }

    ctx
  }

  def displayHelp() {
    println("Usage: ./slacc [options] <file>")
    println("Options include:")
    println(" --help        displays this help")
    println(" --tokens      displays the list of tokens")
    println(" -d <outdir>   generates class files in the specified directory")
  }

  def main(args: Array[String]) {
    val ctx = processOptions(args)

    if (ctx.doTokens) {
      val iter = Lexer.run(ctx)(ctx.files.head)
      while (iter.hasNext) {
        val n = iter.next()
        println(n+"("+n.line+":"+n.col+")")
      }
    } else {
      val pipeline = Lexer
      val it = pipeline.run(ctx)(ctx.files.head)
      while (it.hasNext) {
        println(it.next)
      }
    }
  }
}
