package slacc
package analyzer

import utils._
import ast.Trees._
import Symbols._

object NameAnalysis extends Pipeline[Program, Program] {

  var glob = new GlobalScope()

  def run(ctx: Context)(prog: Program): Program = {
    import ctx.reporter._

    var mainClassDecl = new ClassDecl(new Identifier("main"), None, List(), List(prog.main.main))

    // Step 1: Collect symbols in declarations
    for (classDecl <- prog.classes :+ mainClassDecl) {
      val classId = classDecl.id.value
      glob.lookupClass(classId) match {
        case Some(s) => printAlreadyDefined(classId, s, classDecl.id, ctx.reporter)
        case None => {
          val classSym = new ClassSymbol(classId)
          classSym.setPos(classDecl)
          classDecl.setSymbol(classSym)
          classDecl.id.setSymbol(classDecl.getSymbol)
          glob.addClass(classId, classDecl.getSymbol)
          classDecl.parent match {
            case Some(p) => {
              glob.lookupClass(p.value) match {
                case None => { printNotDeclared(p.value, p, ctx.reporter) }
                case Some(c) => {
                  classDecl.getSymbol.parent = Some(c)
                  if (hasInheritanceCycle(classDecl.getSymbol)) {
                    ctx.reporter.error("cycles in the inheritance graph", p)
                  }
                }
              }
            }
            case None => { }
          }
        }
      }
    }

    for (classDecl <- prog.classes :+ mainClassDecl) {
      for (classVar <- classDecl.vars) {
        val varID = classVar.id.value
        glob.classes(classDecl.id.value).lookupVar(varID) match {
          case Some(s) => printAlreadyDefined(varID, s, classVar.id, ctx.reporter)
          case None => {
            checkClassType(classVar, ctx.reporter)
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
          case Some(s) => printAlreadyDefined(methodId, s, method.id, ctx.reporter)
          case None => {
            classDecl.parent match {
              case Some(p) => {
                glob.classes(p.value).lookupMethod(methodId) match {
                  case Some(m) => {
                    println(m.argList)
                    println(method.args)
                    if (m.argList.length == method.args.length) {
                      // OK!!
                    } else {
                      ctx.reporter.error("Method is already defined in superclass " + p.value, method)
                    }
                  }
                  case None => {

                  }
                }
              }
              case None => { }
            }
            var methodSym = new MethodSymbol(method.id.value, classDecl.getSymbol)
            methodSym.setPos(method)
            method.setSymbol(methodSym)
            method.id.setSymbol(method.getSymbol)
            classDecl.getSymbol.addMethod(methodId, method.getSymbol)
          }
        }
      }
    }

    for (classDecl <- prog.classes :+ mainClassDecl) {
      for (method <- classDecl.methods) {
        for (param <- method.args) {
          val paramId = param.id.value
          glob.classes(classDecl.id.value).methods(method.id.value).lookupVar(paramId) match {
            case Some(s) => printAlreadyDefined(paramId, s, param.id, ctx.reporter)
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
            case Some(s) => printAlreadyDefined(methodVarId, s, methodVar.id, ctx.reporter)
            case None => {
              checkClassType(methodVar, ctx.reporter)
              var methodVarSym = new VariableSymbol(methodVar.id.value)
              methodVarSym.setPos(methodVar)
              methodVar.setSymbol(methodVarSym)
              methodVar.id.setSymbol(methodVar.getSymbol)
              method.getSymbol.addMember(methodVarId, methodVar.getSymbol)
            }
          }
        }

        classDecl.parent match {
          case Some(p) => {
            attachIdentifier(p)
          }
          case None => { }
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
            case Minus(lhs, rhs) => {
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
                case None => {
                  printNotDeclared(meth.value, meth, ctx.reporter)
                }
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
                case Some(s) => {
                  // There is a symbol named value in the method scope
                  t.asInstanceOf[Identifier].setSymbol(s)
                }
                case None => {
                  val classSym = sym.classSymbol
                  classSym.lookupVar(value) match {
                    case Some(ss) => {
                      // THere is a symbol named value in the class scope
                      t.asInstanceOf[Identifier].setSymbol(ss)
                    }
                    case None => {
                      // Variable value not define in method or class
                      glob.lookupClass(value) match {
                        case Some(sss) => {
                          t.asInstanceOf[Identifier].setSymbol(sss)
                        }
                        case None => {
                          printNotDeclared(value, t, ctx.reporter)
                        }
                      }
                    }
                  }
                }
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

  def printAlreadyDefined(n: String, definedAt: Positioned, pos: Positioned, rep: Reporter): Unit = {
    rep.error(n + " already defined at " + definedAt.position, pos)
  }

  def printNotDeclared(n: String, obj: ExprTree, reporter: Reporter): Unit = {
    reporter.error(n + " is not declared", obj)
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

  def checkClassType(varDecl: VarDecl, reporter: Reporter): Unit = {
    if (varDecl.tpe.isInstanceOf[Identifier]) {
      glob.lookupClass(varDecl.tpe.asInstanceOf[Identifier].value) match {
        case None => {
          reporter.error("Class " + varDecl.tpe.asInstanceOf[Identifier].value + " is not defined", varDecl)
        }
        case _ => {}
      }
    }
  }
}
