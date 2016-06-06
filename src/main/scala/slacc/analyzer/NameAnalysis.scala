package slacc
package analyzer

import utils._
import ast.Trees._
import Symbols._
import Types._

object NameAnalysis extends Pipeline[Program, Program] {

  var glob = new GlobalScope()
  var unusedVariables = scala.collection.mutable.Set[Symbol]()
  var typeInferredMethods = scala.collection.mutable.Set[Symbol]()
  var mainClassDecl = new ClassDecl(new Identifier("Main"), None, List(), List())

  def run(ctx: Context)(prog: Program): Program = {
    import ctx.reporter._

    mainClassDecl = new ClassDecl(new Identifier("Main"), None, List(), List(prog.main.main))

    def parseMethod(method: MethodDecl, cd: ClassDecl): Type = {
      if (method.id.hasSymbol) {
        if (typeInferredMethods contains method.id.getSymbol) {
          return method.id.getSymbol.getType
        }
      }
      val methodId = method.id.value
      glob.classes(cd.id.value).lookupMethod(methodId) match {
        case Some(s) => {
          // The method is previously defined (as in, a parent has it)
          if (cd.methods.contains(s)) {
            printAlreadyDefined(methodId, method.id, ctx.reporter)
          } else {
            // Check if overriding
            if (s.argList.length == method.args.length
                && sameParameterTypes(method.args, s.argList, ctx.reporter)
                && sameReturnTypes(method, s, ctx.reporter)) {
              var methodSym = new MethodSymbol(method.id.value, cd.getSymbol)
              methodSym.setPos(method)
              // If no type supplied, we set TUntyped and type it later
              methodSym.setType(getTypeOfTypeTree(method.retType, ctx.reporter))
              methodSym.overridden = Some(s)
              methodSym.setDeclaration(method)
              method.setSymbol(methodSym)
              method.id.setSymbol(method.getSymbol)
              cd.getSymbol.addMethod(methodId, method.getSymbol)

              for (param <- method.args) {
                val paramId = param.id.value
                glob.classes(cd.id.value).methods(method.id.value).lookupVar(paramId) match {
                  case Some(s) => printAlreadyDefined(paramId, param.id, ctx.reporter)
                  case None => {
                    createParameterSymbol(param)
                    method.getSymbol.addParam(paramId, param.getSymbol)
                    unusedVariables += param.getSymbol
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
          var methodSym = new MethodSymbol(method.id.value, cd.getSymbol)
          methodSym.setPos(method)
          methodSym.setDeclaration(method)
          method.setSymbol(methodSym)
          methodSym.setType(getTypeOfTypeTree(method.retType, ctx.reporter))
          method.id.setSymbol(method.getSymbol)
          cd.getSymbol.addMethod(methodId, method.getSymbol)

          for (param <- method.args) {
            val paramId = param.id.value
            glob.classes(cd.id.value).methods(method.id.value).lookupVar(paramId) match {
              case Some(s) => printAlreadyDefined(paramId, param.id, ctx.reporter)
              case None => {
                createParameterSymbol(param)
                method.getSymbol.addParam(paramId, param.getSymbol)
                unusedVariables += param.getSymbol
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
      method.getSymbol.getType
    }

    def parseMethodVar(methodVar: VarDecl, method: MethodDecl, cd: ClassDecl): Unit = {
      if (method.id.hasSymbol) {
        if (typeInferredMethods contains method.id.getSymbol) {
          return
        }
      }
      val methodVarId = methodVar.id.value
      glob.classes(cd.id.value).methods(method.id.value).lookupVar(methodVarId) match {
        case Some(s) => printAlreadyDefined(methodVarId, methodVar.id, ctx.reporter)
        case None => {
          checkClassType(methodVar, ctx.reporter)
          var methodVarSym = new VariableSymbol(methodVar.id.value)
          methodVarSym.setPos(methodVar)
          methodVarSym.setType(getTypeOfTypeTree(methodVar.tpe, ctx.reporter))
          methodVar.tpe match {
            case Identifier(value) => {
              // Class type
              attachIdentifier(methodVar.tpe.asInstanceOf[Identifier])(method, cd)
            }
            case _ => { /* Primitive type */ }
          }
          methodVar.setSymbol(methodVarSym)
          methodVar.id.setSymbol(methodVar.getSymbol)
          method.getSymbol.addMember(methodVarId, methodVar.getSymbol)
          methodVar.expr match {
            case Some(e) => {
              attachIdentifier(methodVar.id)(method, cd)
              attachIdentifier(new Assign(methodVar.id, e))(method, cd)
              methodVar.getSymbol.setType(getTypeOfExprTree(e))
            }
            case None => unusedVariables += methodVar.getSymbol
          }
        }
      }
    }

    def createParameterSymbol(formal: Formal): VariableSymbol = {
      var symbol = new VariableSymbol(formal.id.value)
      symbol.setPos(formal)
      symbol.setType(getTypeOfTypeTree(formal.tpe, ctx.reporter))
      formal.setSymbol(symbol)
      formal.id.setSymbol(symbol)
      symbol
    }

    def attachIdentifier(t: ExprTree)(implicit method: MethodDecl, cd: ClassDecl): Unit = {

      /* Lots of expressions take two expressions (And, Or, Plus, Minus, etc.)
      we give these a special trait (asTuple) and use dualAttachment to
      attach identifiers to them all. Saves a lot of space */
      def dualAttachment(t1: ExprTree, t2: ExprTree)(implicit method: MethodDecl, cd: ClassDecl): Unit = {
        attachIdentifier(t1)(method, cd)
        attachIdentifier(t2)(method, cd)
      }

      t match {
        case tuple: AsTuple => {
          dualAttachment(tuple.lhs, tuple.rhs)(method,cd)
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
          obj match {
            case Self() => {
              // If object is self, check for the method in the class
              obj.asInstanceOf[Self].getSymbol.lookupMethod(meth.value) match {
                case Some(ms) => {
                  ms.getType match {
                    // There is a method in the class but it has not yet
                    // been typed
                    case TUntyped => {
                      // Parse the MethodDecl to find the return expression type
                      ms.getDeclaration match {
                        case Some(md) => {
                          obj.asInstanceOf[Self].getSymbol.getDeclaration match {
                            case Some(cd) => {
                              parseMethod(md, cd)
                              md.vars foreach { mdv => parseMethodVar(mdv, md, cd) }
                              md.exprs foreach { mde => attachIdentifier(mde)(md, cd) }
                              attachIdentifier(md.retExpr)(md, cd)
                              md.id.getSymbol.setType(getTypeOfExprTree(md.retExpr))
                              meth.setSymbol(md.id.getSymbol)
                              typeInferredMethods += meth.getSymbol
                            }
                            case None => { sys.error(s"Self is of " +
                              "class " +
                              "${obj.asInstanceOf[Self].getSymbol.name} " +
                              "but does not have a declaration attached " +
                              " to it") }
                          }
                        }
                        case None => sys.error("No declaration connected to " + ms.name)
                      }
                    }
                    case _ => meth.setSymbol(ms)
                  }
                }
                case None => { ctx.reporter.error("No method " + meth.value + " defined") }
              }
            }
            case Identifier(value) => {
              glob.classes(obj.asInstanceOf[Identifier].getSymbol.getType.toString).lookupMethod(meth.value) match {
                case Some(ms) => {
                  ms.getType match {
                    case TUntyped => {
                      ms.getDeclaration match {
                        case Some(md) => {
                          ms.classSymbol.getDeclaration match {
                            case Some(cd) => {
                              parseMethod(md, cd)
                              md.vars foreach { mdv => parseMethodVar(mdv, md, cd) }
                              md.exprs foreach { mde => attachIdentifier(mde)(md, cd) }
                              attachIdentifier(md.retExpr)(md, cd)
                              md.id.getSymbol.setType(getTypeOfExprTree(md.retExpr))
                              meth.setSymbol(md.id.getSymbol)
                              println(meth + ": " + meth.getSymbol.getType)
                              typeInferredMethods += meth.getSymbol
                            }
                            case None => { sys.error(s"Self is of " +
                              "class " +
                              "${obj.asInstanceOf[Self].getSymbol.name} " +
                              "but does not have a declaration attached " +
                              " to it") }
                          }
                        }
                        case None => sys.error("No declaration connected to " + ms.name)
                      }
                    }
                    case _ => meth.setSymbol(ms)
                  }
                }
                case None => ctx.reporter.error("No method " + meth.value + " defined")
              }
            }
            case _ => { sys.error("You w0t son?") }
          }
          for (arg <- args) {
            attachIdentifier(arg)
          }
        }
        case Self() => {
          t.asInstanceOf[Self].setSymbol(cd.getSymbol)
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

    def createClassSymbol(classDecl: ClassDecl): ClassSymbol = {
      val symbol = new ClassSymbol(classDecl.id.value)
      symbol.setPos(classDecl)
      symbol.setType(TClass(symbol))
      symbol.setDeclaration(classDecl)
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
            unusedVariables += classVar.getSymbol
          }
        }
      }

      classDecl.methods foreach { m => parseMethod(m, classDecl) }
    }

    for (classDecl <- prog.classes :+ mainClassDecl) {
      for (method <- classDecl.methods) {
        for (classVar <- classDecl.vars) {
          unusedVariables += classVar.getSymbol
          classVar.tpe match {
            case Identifier(value) => {
              // Class type
              attachIdentifier(classVar.tpe.asInstanceOf[Identifier])(method, classDecl)
            }
            case _ => { /* Primitive type */ }
          }
        }

        method.vars foreach { mv => parseMethodVar(mv, method, classDecl)}

        for (param <- method.args) {
          param.tpe match {
            case Identifier(_) => {
              attachIdentifier(param.tpe.asInstanceOf[Identifier])(method, classDecl)
            }
            case _ => { }
          }
        }

        classDecl.parent match {
          case Some(p) => {
            attachIdentifier(p)(method, classDecl)
          }
          case None => { }
        }

        for (expr <- method.exprs) {
          attachIdentifier(expr)(method, classDecl)
        }

        attachIdentifier(method.retExpr)(method, classDecl)

        method.retType match {
          case UntypedType() => {
            method.id.getSymbol.setType(getTypeOfExprTree(method.retExpr))
            method.getSymbol.setType(getTypeOfExprTree(method.retExpr))
          }
          case _ => { }
        }

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
        typeInferredMethods += method.getSymbol
      }
    }

    // Step 2: Attach symbols to identifiers (except method calls) in method bodies
    // (Step 3:) Print tree with symbol ids for debugging

    // Make sure you check all constraints

    unusedVariables foreach { uv => ctx.reporter.warning(s"Variable ${uv.name} is not used", uv)}

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

  def getTypeOfExprTree(e: ExprTree): Type = {
    e match {
      case And(lhs, rhs) => TBoolean
      case Or(lhs, rhs) => TBoolean
      case Plus(lhs, rhs) => {
        getTypeOfExprTree(lhs) match {
          case TInt => getTypeOfExprTree(rhs)
          case TString => TString
          case _ => sys.error(s"Addition not supported for ${lhs}:${getTypeOfExprTree(lhs)}")
        }
      }
      case Minus(lhs, rhs) => TInt
      case Times(lhs, rhs) => TInt
      case Div(lhs, rhs) => TInt
      case LessThan(lhs, rhs) => TBoolean
      case Equals(lhs, rhs) => TBoolean
      case ArrayRead(arr, idx) => TInt
      case ArrayLength(arr) => TInt
      case MethodCall(obj, meth: Identifier, args) => meth.getSymbol.getType
      case IntLit(value) => TInt
      case StringLit(value) => TInt
      case True() => TBoolean
      case False() => TBoolean
      case Identifier(id) => e.getType
      case NewIntArray(size) => TIntArray
      case New(tpe) => TClass(glob.classes(tpe.value))
      case Not(expr) => TBoolean
      case Block(exprs) => getTypeOfExprTree(exprs.last)
      case While(cond, body) => TUnit
      case Println(expr) => TUnit
      case Assign(id, expr) => TUnit
      case ArrayAssign(arr, idx, expr) => TUnit
      case Strof(expr) => TString
      case _ => sys.error(s"Not handling ${e}")
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
