package slacc
package analyzer

import utils._
import ast.Trees._
import Symbols._

object NameAnalysis extends Pipeline[Program, Program] {

  def run(ctx: Context)(prog: Program): Program = {
    import ctx.reporter._

    // Step 1: Collect symbols in declarations
    for (classDecl <- prog.classes) {
        classDecl.setSymbol(new ClassSymbol(classDecl.id.value))

        for (classVar <- classDecl.vars) {
          classVar.setSymbol(new VariableSymbol(classVar.id.value))
        }

        for (method <- classDecl.methods) {
          method.setSymbol(new MethodSymbol(method.id.value, classDecl.getSymbol))

          for (methodVar <- method.vars) {
            methodVar.setSymbol(new VariableSymbol(methodVar.id.value))
          }
        }
    }
    // Step 2: Attach symbols to identifiers (except method calls) in method bodies
    // (Step 3:) Print tree with symbol ids for debugging

    // Make sure you check all constraints

    prog
  }
}
