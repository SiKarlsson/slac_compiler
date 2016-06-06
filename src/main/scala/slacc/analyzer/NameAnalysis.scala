package slacc
package analyzer

import utils._
import ast.Trees._
import Symbols._
import Types._

object NameAnalysis extends Pipeline[Program, Program] {

  var glob = new GlobalScope()
  var unusedVariables = scala.collection.mutable.Map[Symbol, Boolean]()
  var mainClassDecl = new ClassDecl(new Identifier("Main"), None, List(), List())

  def run(ctx: Context)(prog: Program): Program = {
    import ctx.reporter._

    mainClassDecl = new ClassDecl(new Identifier("Main"), None, List(), List(prog.main.main))

    def createParameterSymbol(formal: Formal): VariableSymbol = {
      var symbol = new VariableSymbol(formal.id.value)
      symbol.setPos(formal)
      symbol.setType(getTypeOfTypeTree(formal.tpe, ctx.reporter))
      formal.setSymbol(symbol)
      formal.id.setSymbol(symbol)
      symbol
    }

    def createClassSymbol(classDecl: ClassDecl): ClassSymbol = {
      val symbol = new ClassSymbol(classDecl.id.value)
      symbol.setPos(classDecl)
      symbol.setType(TClass(symbol))
      classDecl.setSymbol(symbol)
      classDecl.id.setSymbol(symbol)
      symbol
    }

    /* Iterate through classes first. Otherwise we risk running into a class
    not yet analysed (for example used as a type) */
    for (classDecl <- prog.classes :+ mainClassDecl) {
      val classId = classDecl.id.value
      glob.lookupClass(classId) match {
        case Some(s) => printAlreadyDefined(classId, classDecl.id, ctx.reporter)
        case None => {
          createClassSymbol(classDecl)
          glob.addClass(classId, classDecl.getSymbol)
        }
      }
    }

    /* Now that all classes has been analysed, we can add parents */
    for (classDecl <- prog.classes :+ mainClassDecl) {
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
      /* Analyse class variables. As a class variable can have another class as
      type, this must be done after analysing all the classes */
      for (classVar <- classDecl.vars) {
        val varID = classVar.id.value
        glob.classes(classDecl.id.value).lookupVar(varID) match {
          case Some(s) => printAlreadyDefined(varID, classVar.id, ctx.reporter)
          case None => {
            checkClassType(classVar, ctx.reporter)
            var classVarSym = new VariableSymbol(classVar.id.value)
            classVarSym.setPos(classVar)
            classVarSym.setType(getTypeOfTypeTree(classVar.tpe, ctx.reporter))
            classVarSym.getType match {
              case TUntyped => { ctx.reporter.error("Trying to infer " +
                "type on class variable. This is not allowed as the variable" +
                " could get different types depending on the methods of the" +
                " class. (Could be a = \"hello\"; in one method and a = 2; in" +
                " another)", classVar) }
              case _ => { }
            }
            classVar.setSymbol(classVarSym)
            classVar.id.setSymbol(classVar.getSymbol)
            classDecl.getSymbol.addMember(varID, classVar.getSymbol)
            unusedVariables += (classVar.getSymbol -> false)
          }
        }
      }

      for (method <- classDecl.methods) {
        val methodId = method.id.value
        glob.classes(classDecl.id.value).lookupMethod(methodId) match {
          case Some(s) => {
            // The method is previously defined (as in, a parent has it)
            if (classDecl.methods.contains(s)) {
              printAlreadyDefined(methodId, method.id, ctx.reporter)
            } else {
              // Check if overriding
              if (s.argList.length == method.args.length
                  && sameParameterTypes(method.args, s.argList, ctx.reporter)
                  && sameReturnTypes(method, s, ctx.reporter)) {
                var methodSym = new MethodSymbol(method.id.value, classDecl.getSymbol)
                methodSym.setPos(method)
                // If no type supplied, we set TUntyped and type it later
                methodSym.setType(getTypeOfTypeTree(method.retType, ctx.reporter))
                methodSym.overridden = Some(s)
                method.setSymbol(methodSym)
                method.id.setSymbol(method.getSymbol)
                classDecl.getSymbol.addMethod(methodId, method.getSymbol)

                for (param <- method.args) {
                  val paramId = param.id.value
                  glob.classes(classDecl.id.value).methods(method.id.value).lookupVar(paramId) match {
                    case Some(s) => printAlreadyDefined(paramId, param.id, ctx.reporter)
                    case None => {
                      createParameterSymbol(param)
                      method.getSymbol.addParam(paramId, param.getSymbol)
                      unusedVariables += (param.getSymbol -> false)
                    }
                  }
                }

                method.retType match {
                  case Identifier(value) => {
                    glob.classes get value match {
                      case Some(cs) => method.retType.asInstanceOf[Identifier].setSymbol(cs)
                      case None => ctx.reporter.error("Class is not declared", method.retType)
                    }
                  }
                  case _ => { }
                }
              } else {
                ctx.reporter.error("Method is already defined in superclass", method)
              }
            }
          }
          case None => {
            // The method is not previously defined
            var methodSym = new MethodSymbol(method.id.value, classDecl.getSymbol)
            methodSym.setPos(method)
            method.setSymbol(methodSym)
            methodSym.setType(getTypeOfTypeTree(method.retType, ctx.reporter))
            method.id.setSymbol(method.getSymbol)
            classDecl.getSymbol.addMethod(methodId, method.getSymbol)

            for (param <- method.args) {
              val paramId = param.id.value
              glob.classes(classDecl.id.value).methods(method.id.value).lookupVar(paramId) match {
                case Some(s) => printAlreadyDefined(paramId, param.id, ctx.reporter)
                case None => {
                  createParameterSymbol(param)
                  method.getSymbol.addParam(paramId, param.getSymbol)
                  unusedVariables += (param.getSymbol -> false)
                }
              }
            }

            method.retType match {
              case Identifier(value) => {
                glob.classes get value match {
                  case Some(cs) => method.retType.asInstanceOf[Identifier].setSymbol(cs)
                  case None => ctx.reporter.error("Class is not declared", method.retType)
                }
              }
              case _ => { }
            }
          }
        }
      }
    }

    for (classDecl <- prog.classes :+ mainClassDecl) {
      for (method <- classDecl.methods) {
        for (classVar <- classDecl.vars) {
          unusedVariables += (classVar.getSymbol -> false)
          classVar.tpe match {
            case Identifier(value) => {
              // Class type
              attachIdentifier(classVar.tpe.asInstanceOf[Identifier])
            }
            case _ => { /* Primitive type */ }
          }
        }
        for (methodVar <- method.vars) {
          val methodVarId = methodVar.id.value
          glob.classes(classDecl.id.value).methods(method.id.value).lookupVar(methodVarId) match {
            case Some(s) => printAlreadyDefined(methodVarId, methodVar.id, ctx.reporter)
            case None => {
              checkClassType(methodVar, ctx.reporter)
              var methodVarSym = new VariableSymbol(methodVar.id.value)
              methodVarSym.setPos(methodVar)
              methodVarSym.setType(getTypeOfTypeTree(methodVar.tpe, ctx.reporter))
              methodVar.tpe match {
                case Identifier(value) => {
                  // Class type
                  attachIdentifier(methodVar.tpe.asInstanceOf[Identifier])
                }
                case _ => { /* Primitive type */ }
              }
              methodVar.setSymbol(methodVarSym)
              methodVar.id.setSymbol(methodVar.getSymbol)
              method.getSymbol.addMember(methodVarId, methodVar.getSymbol)
              methodVar.expr match {
                case Some(e) => {
                  attachIdentifier(methodVar.id)
                  attachIdentifier(new Assign(methodVar.id, e))
                }
                case None => unusedVariables += (methodVar.getSymbol -> false)
              }
            }
          }
        }

        for (param <- method.args) {
          param.tpe match {
            case Identifier(_) => {
              attachIdentifier(param.tpe.asInstanceOf[Identifier])
            }
            case _ => { }
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

        def getSymbolFromObj(obj: ExprTree): Symbol = {
          obj match {
            case New(tpe) => {
              tpe.getSymbol
            }
            case Self() => {
              obj.asInstanceOf[Self].getSymbol
            }
            case MethodCall(obj, meth, args) => {
              getSymbolFromObj(obj)
            }
            case Identifier(id) => {
              obj.asInstanceOf[Identifier].getSymbol
            }
            case _ => { ctx.reporter.error("Can't use method call on " + obj, obj); ??? }
          }
        }

        /* Lots of expressions take two expressions (And, Or, Plus, Minus, etc.)
        we give these a special trait (asTuple) and use dualAttachment to
        attach identifiers to them all. Saves a lot of space */
        def dualAttachment(t1: ExprTree, t2: ExprTree): Unit = {
          attachIdentifier(t1)
          attachIdentifier(t2)
        }

        def attachIdentifier(t: ExprTree): Unit = {
          t match {
            case tuple: AsTuple => {
              dualAttachment(tuple.lhs, tuple.rhs)
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
              for (arg <- args) {
                attachIdentifier(arg)
              }
            }
            case Self() => {
              t.asInstanceOf[Self].setSymbol(classDecl.getSymbol)
            }
            case NewIntArray(size) => attachIdentifier(size)
            case New(id) => attachIdentifier(id)
            case Not(expr) => attachIdentifier(expr)
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
            case Assign(id: Identifier, expr) => {
              attachIdentifier(id)
              attachIdentifier(expr)
              unusedVariables -= id.getSymbol
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
              if (method.asInstanceOf[MethodDecl].hasSymbol) {
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
                        // There is a symbol named value in the class scope
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
            }
            case _ => {  }
          }
        }
      }
    }

    // Step 2: Attach symbols to identifiers (except method calls) in method bodies
    // (Step 3:) Print tree with symbol ids for debugging

    // Make sure you check all constraints

    for (k <- unusedVariables) {
      ctx.reporter.warning(s"Variable ${k._1.name} is not used", k._1)
    }

    prog
  }

  def printAlreadyDefined(n: String, pos: Positioned, rep: Reporter): Unit = {
    rep.error(n + " already defined", pos)
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

  def getTypeOfTypeTree(t: TypeTree, rep: Reporter): Type = {
    t match {
      case IntType() => TInt
      case StringType() => TString
      case UnitType() => TUnit
      case BooleanType() => TBoolean
      case IntArrayType() => TIntArray
      case UntypedType() => TUntyped
      case Identifier(value) => {
        glob.lookupClass(value) match {
          case Some(c) => TClass(c)
          case None => {
            rep.error(value + " can't be used as type", t)
            TUntyped
          }
        }
      }
      case _ => {
        sys.error(t + " has no type!")
      }
    }
  }

  def checkClassType(varDecl: VarDecl, rep: Reporter): Unit = {
    if (varDecl.tpe.isInstanceOf[Identifier]) {
      glob.lookupClass(varDecl.tpe.asInstanceOf[Identifier].value) match {
        case None => {
          rep.error("Class " + varDecl.tpe.asInstanceOf[Identifier].value + " is not defined", varDecl)
        }
        case _ => {}
      }
    }
  }

  def sameParameterTypes(args: List[Formal], argList: List[VariableSymbol], rep: Reporter): Boolean = {
    for (i <- 0 until args.length) {
      if (getTypeOfTypeTree(args(i).tpe, rep) != argList(i).getType) {
        return false
      }
    }
    true
  }

  def sameReturnTypes(meth: MethodDecl, parent: MethodSymbol, rep: Reporter): Boolean = {
    if (getTypeOfTypeTree(meth.retType, rep) == parent.getType) {
      true
    } else {
      false
    }
  }
}
