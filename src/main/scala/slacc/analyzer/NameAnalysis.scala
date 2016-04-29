package slacc
package analyzer

import utils._
import ast.Trees._
import Symbols._

object NameAnalysis extends Pipeline[Program, Program] {

  var glob = new GlobalScope()

  def run(ctx: Context)(prog: Program): Program = {
    import ctx.reporter._

    prog.main.setSymbol(new ClassSymbol("Main"))
    println("MM--".concat(prog.main.getSymbol.id.toString))

    // Step 1: Collect symbols in declarations
    for (classDecl <- prog.classes) {
      println(classDecl.id)
      val classId = classDecl.id.value
      glob.lookupClass(classId) match {
        case Some(s) => printAlreadyDefined(classId)
        case None => {
          classDecl.setSymbol(new ClassSymbol(classId))
          classDecl.id.setSymbol(classDecl.getSymbol)
          glob.addClass(classId, classDecl.getSymbol)
          classDecl.parent match {
            case Some(p) => {
              glob.lookupClass(p.value) match {
                case None => { error(p.value + " not defined (Parent of " + classId + ")") }
                case Some(c) => {
                  classDecl.getSymbol.parent = Some(c)
                  if (hasInheritanceCycle(classDecl.getSymbol)) {
                    error("cycles in the inheritance graph")
                  }
                }
              }
            }
            case None => { }
          }
        }
      }
      println("C--".concat(classDecl.getSymbol.id.toString))

      for (classVar <- classDecl.vars) {
        val varID = classVar.id.value
        glob.classes(classDecl.id.value).lookupVar(varID) match {
          case Some(s) => printAlreadyDefined(varID)
          case None => {
            classVar.setSymbol(new VariableSymbol(classVar.id.value))
            classVar.id.setSymbol(classVar.getSymbol)
            glob.classes(classDecl.id.value).addMember(varID, classVar.getSymbol)
          }
        }
        println("CV--".concat(classVar.getSymbol.id.toString))
      }

      for (method <- classDecl.methods) {
        val methodId = method.id.value
        glob.classes(classDecl.id.value).lookupMethod(methodId) match {
          case Some(s) => printAlreadyDefined(methodId)
          case None => {
            method.setSymbol(new MethodSymbol(method.id.value, classDecl.getSymbol))
            method.id.setSymbol(method.getSymbol)
            glob.classes(classDecl.id.value).addMethod(methodId, method.getSymbol)
          }
        }
        println("M--".concat(method.getSymbol.id.toString))

        for (param <- method.args) {
          val paramId = param.id.value
          glob.classes(classDecl.id.value).methods(method.id.value).lookupVar(paramId) match {
            case Some(s) => printAlreadyDefined(paramId)
            case None => {
              param.setSymbol(new VariableSymbol(param.id.value))
              param.id.setSymbol(param.getSymbol)
              glob.classes(classDecl.id.value).methods(method.id.value).addParam(paramId, param.getSymbol)
            }
          }
          println("MP--".concat(param.getSymbol.id.toString))
        }

        for (methodVar <- method.vars) {
          val methodVarId = methodVar.id.value
          glob.classes(classDecl.id.value).methods(method.id.value).lookupVar(methodVarId) match {
            case Some(s) => printAlreadyDefined(methodVarId)
            case None => {
              methodVar.setSymbol(new VariableSymbol(methodVar.id.value))
              methodVar.id.setSymbol(methodVar.getSymbol)
              glob.classes(classDecl.id.value).methods(method.id.value).addMember(methodVarId, methodVar.getSymbol)
            }
          }
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

  def printAlreadyDefined(n: String): Unit = {
    error(n + " already defined")
  }

  def hasInheritanceCycle(c: ClassSymbol): Boolean = {
    var parent: Option[ClassSymbol] = c.parent
    while (parent != None) {
      if (c.name == parent.get.name) {
        return true
      }
      parent = parent.get.parent
    }
    false
  }
}
