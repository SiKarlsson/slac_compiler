package slacc
package analyzer

import utils._
import ast.Trees._
import Symbols._

object NameAnalysis extends Pipeline[Program, Program] {

  def run(ctx: Context)(prog: Program): Program = {
    import ctx.reporter._

    var glob = new GlobalScope()

    prog.main.setSymbol(new ClassSymbol("Main"))
    println("MM--".concat(prog.main.getSymbol.id.toString))

    // Step 1: Collect symbols in declarations
    for (classDecl <- prog.classes) {
      val classId = classDecl.id.value
      glob.lookupClass(classId) match {
        case Some(s) => {
          println(classId, " already defined")
        }
        case None => {
          classDecl.setSymbol(new ClassSymbol(classId))
          glob.addClass(classId, classDecl.getSymbol)
        }
      }
      println("C--".concat(classDecl.getSymbol.id.toString))

      for (classVar <- classDecl.vars) {
        classVar.setSymbol(new VariableSymbol(classVar.id.value))
        println("CV--".concat(classVar.getSymbol.id.toString))
      }

      for (method <- classDecl.methods) {
        method.setSymbol(new MethodSymbol(method.id.value, classDecl.getSymbol))
        println("M--".concat(method.getSymbol.id.toString))

        for (arg <- method.args) {
          arg.setSymbol(new VariableSymbol(arg.id.value))
          println("MP--".concat(arg.getSymbol.id.toString))
        }

        for (methodVar <- method.vars) {
          methodVar.setSymbol(new VariableSymbol(methodVar.id.value))
          println("MV--".concat(methodVar.getSymbol.id.toString))
        }

        for (expr <- method.exprs) {

        }

        // TODO: Handle retexpr of method
      }
    }

    // Step 2: Attach symbols to identifiers (except method calls) in method bodies
    // (Step 3:) Print tree with symbol ids for debugging

    // Make sure you check all constraints

    prog
  }
}
