package slacc
package analyzer

import utils._
import ast.Trees._
import Symbols._

object NameAnalysis extends Pipeline[Program, Program] {

  var glob = new GlobalScope()

  def run(ctx: Context)(prog: Program): Program = {
    import ctx.reporter._

    val mainClass = new ClassSymbol("main")
    val mainSymbol = new MethodSymbol("main", mainClass)
    mainClass.addMethod("main", mainSymbol)
    println(prog.main)
    prog.main.main.id.setSymbol(mainSymbol)
    glob.addClass("main", mainClass)

    // Step 1: Collect symbols in declarations
    for (classDecl <- prog.classes) {
      println(classDecl.id)
      val classId = classDecl.id.value
      glob.lookupClass(classId) match {
        case Some(s) => printAlreadyDefined(classId, s, ctx.reporter)
        case None => {
          val classSym = new ClassSymbol(classId)
          classSym.setPos(classDecl)
          classDecl.setSymbol(classSym)
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
    }

    for (classDecl <- prog.classes) {
      for (classVar <- classDecl.vars) {
        val varID = classVar.id.value
        glob.classes(classDecl.id.value).lookupVar(varID) match {
          case Some(s) => printAlreadyDefined(varID, s, ctx.reporter)
          case None => {
            var classVarSym = new VariableSymbol(classVar.id.value)
            classVarSym.setPos(classVarSym)
            classVar.setSymbol(classVarSym)
            classVar.id.setSymbol(classVar.getSymbol)
            classDecl.getSymbol.addMember(varID, classVar.getSymbol)
          }
        }
      }

      for (method <- classDecl.methods) {
        val methodId = method.id.value
        glob.classes(classDecl.id.value).lookupMethod(methodId) match {
          case Some(s) => printAlreadyDefined(methodId, s, ctx.reporter)
          case None => {
            var methodSym = new MethodSymbol(method.id.value, classDecl.getSymbol)
            methodSym.setPos(method)
            method.setSymbol(methodSym)
            method.id.setSymbol(method.getSymbol)
            classDecl.getSymbol.addMethod(methodId, method.getSymbol)
          }
        }
      }
    }

    for (classDecl <- prog.classes) {
      for (method <- classDecl.methods) {
        for (param <- method.args) {
          val paramId = param.id.value
          glob.classes(classDecl.id.value).methods(method.id.value).lookupVar(paramId) match {
            case Some(s) => printAlreadyDefined(paramId, s, ctx.reporter)
            case None => {
              var paramSymbol = new VariableSymbol(param.id.value)
              paramSymbol.setPos(param)
              param.setSymbol(paramSymbol)
              param.id.setSymbol(param.getSymbol)
              method.getSymbol.addParam(paramId, param.getSymbol)
            }
          }
        }

        for (methodVar <- method.vars) {
          val methodVarId = methodVar.id.value
          glob.classes(classDecl.id.value).methods(method.id.value).lookupVar(methodVarId) match {
            case Some(s) => printAlreadyDefined(methodVarId, s, ctx.reporter)
            case None => {
              var methodVarSym = new VariableSymbol(methodVar.id.value)
              methodVarSym.setPos(methodVar)
              methodVar.setSymbol(methodVarSym)
              methodVar.id.setSymbol(methodVar.getSymbol)
              method.getSymbol.addMember(methodVarId, methodVar.getSymbol)
            }
          }
        }

        for (expr <- method.exprs) {
          attachIdentifier(expr)
        }

        attachIdentifier(method.retExpr)

        def attachIdentifier(t: ExprTree): Unit = {
          t match {
            case And(lhs, rhs) => {
              attachIdentifier(lhs)
              attachIdentifier(rhs)
            }
            case Or(lhs, rhs) => {
              attachIdentifier(lhs)
              attachIdentifier(rhs)
            }
            case Plus(lhs, rhs) => {
              attachIdentifier(lhs)
              attachIdentifier(rhs)
            }
            case Times(lhs, rhs) => {
              attachIdentifier(lhs)
              attachIdentifier(rhs)
            }
            case Div(lhs, rhs) => {
              attachIdentifier(lhs)
              attachIdentifier(rhs)
            }
            case LessThan(lhs, rhs) => {
              attachIdentifier(lhs)
              attachIdentifier(rhs)
            }
            case Equals(lhs, rhs) => {
              attachIdentifier(lhs)
              attachIdentifier(rhs)
            }
            case ArrayRead(arr, index) => {
              attachIdentifier(arr)
              attachIdentifier(index)
            }
            case ArrayLength(arr) => {
              attachIdentifier(arr)
            }
            case MethodCall(obj, meth, args) => {
              attachIdentifier(obj)
              classDecl.getSymbol.lookupMethod(meth.value) match {
                case Some(s) => { meth.asInstanceOf[Identifier].setSymbol(s) }
                case None => { error(meth + " is not defined in this scope") }
              }
              for (arg <- args) {
                attachIdentifier(arg)
              }
            }
            case Self() => {
              t.asInstanceOf[Self].setSymbol(classDecl.getSymbol)
            }
            case NewIntArray(size) => {
              attachIdentifier(size)
            }
            case New(id) => {
              attachIdentifier(id)
            }
            case Not(expr) => {
              attachIdentifier(expr)
            }
            case Block(exprList) => {
              for (expr <- exprList) {
                attachIdentifier(expr)
              }
            }
            case If(expr, thn, els) => {
              attachIdentifier(expr)
              attachIdentifier(thn)
              els match {
                case Some(e) => attachIdentifier(e)
                case None => { }
              }
            }
            case While(cond, body) => {
              attachIdentifier(cond)
              attachIdentifier(body)
            }
            case Println(expr) => {
              attachIdentifier(expr)
            }
            case Assign(id, expr) => {
              attachIdentifier(id)
              attachIdentifier(expr)
            }
            case ArrayAssign(id, index, expr) => {
              attachIdentifier(id)
              attachIdentifier(index)
              attachIdentifier(expr)
            }
            case Strof(expr) => {
              attachIdentifier(expr)
            }
            case Identifier(value) => {
              val sym = method.asInstanceOf[MethodDecl].getSymbol
              sym.lookupVar(value) match {
                case Some(s) => { t.asInstanceOf[Identifier].setSymbol(s) }
                case None => { error(value + " is not defined in this scope") }
              }
            }
            case _ => {  }
          }
        }
      }
    }

    // Step 2: Attach symbols to identifiers (except method calls) in method bodies
    // (Step 3:) Print tree with symbol ids for debugging

    // Make sure you check all constraints

    prog
  }

  def printAlreadyDefined(n: String, pos: Positioned, rep: Reporter): Unit = {
    println(pos.position)
    rep.error(n + " already defined", pos)
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
