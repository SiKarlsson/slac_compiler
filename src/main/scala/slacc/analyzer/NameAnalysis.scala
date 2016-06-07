package slacc
package analyzer

import utils._
import ast.Trees._
import Symbols._
import Types._

object NameAnalysis extends Pipeline[Program, Program] {

  var glob = new GlobalScope()

  /**
    Variables - put all variables into this set
    ReadVariables - when a variable is read, put it into this set
    writtenVariables - when a variable is written to, put it into this set

    If a variable is in Variables but not in both ReadVariables and
      writtenVariables, it is considered unused.
   */
  var variables = scala.collection.mutable.Set[Symbol]()
  var readVariables = scala.collection.mutable.Set[Symbol]()
  var writtenVariables = scala.collection.mutable.Set[Symbol]()

  /** typeInferredMethods is the set of methods that has been type inferred */
  var typeInferredMethods = scala.collection.mutable.Set[Symbol]()
  var mainClassDecl = new ClassDecl(new Identifier("Main"), None, List(), List())

  def run(ctx: Context)(prog: Program): Program = {
    import ctx.reporter._

    mainClassDecl = new ClassDecl(new Identifier("Main"), None, List(), List(prog.main.main))

    /**
     * Traverses the method. Creates symbols for all variables and attaches
     * identifiers to the expressions (incl return expression). It then tries
     * to determine the return type of the method
     */
    def inferReturnType(methodDecl: MethodDecl, classDecl: ClassDecl): Type = {
      if (typeInferredMethods contains methodDecl.id.getSymbol) {
        methodDecl.id.getSymbol.getType
      } else {
        /** We need to go through the whole method in case the retExpr contains
          a variable that needs to be type inferred */
        methodDecl.vars foreach (mv => analyseMethodVariable(mv, methodDecl, classDecl) )
        methodDecl.exprs foreach (me => attachIdentifier(me)(methodDecl, classDecl) )
        attachIdentifier(methodDecl.retExpr)(methodDecl, classDecl)
        typeInferredMethods += methodDecl.id.getSymbol
        methodDecl.retType match {
          case UntypedType() => getTypeOfExprTree(methodDecl.retExpr)
          case _ => getTypeOfTypeTree(methodDecl.retType, ctx.reporter)
        }
      }
    }

    /**
     * Creates a method symbol. If the method returns an identifier, the value
     * is checked to be an existing class.
     */
    def createMethodSymbol(methodDecl: MethodDecl, classDecl: ClassDecl): MethodSymbol = {
      var methodSymbol = new MethodSymbol(methodDecl.id.value, classDecl.getSymbol)
      methodSymbol.setPos(methodDecl)
      methodDecl.retType match {
        case Identifier(value) => {
          glob.classes get value match {
            case Some(cs) => methodDecl.retType.asInstanceOf[Identifier].setSymbol(cs)
            case None => ctx.reporter.error("Class is not declared", methodDecl.retType)
          }
        }
        case UntypedType() => { }
        case _ => methodSymbol.setType(getTypeOfTypeTree(methodDecl.retType, ctx.reporter))
      }
      methodSymbol.setDeclaration(methodDecl)
      methodSymbol
    }

    /**
     * Performs name analysis on a method. This includes checking that there is
     * no other method in the class with the same name, check whether the method
     * is overriding a method in the superclass, and create symbols for the
     * parameters.
     */
    def analyseMethod(methodDecl: MethodDecl, classDecl: ClassDecl): Unit = {
      val methodId = methodDecl.id;
      if (methodId.hasSymbol) {
        if (typeInferredMethods contains methodId.getSymbol) {
          return
        }
      }

      /* Is the method defined? (either in same class => error, or in superclass?) */
      classDecl.getSymbol.lookupMethod(methodId.value) match {
        case Some(m) => {
          if (classDecl.methods contains methodId.value) {
            /* The method is already defined in this class */
            printAlreadyDefined(methodId.value, methodDecl.id, ctx.reporter)
          } else {
            /* The method exists in a superclass, check overriding */
            if (m.argList.length == methodDecl.args.length
                && sameParameterTypes(methodDecl.args, m.argList, ctx.reporter)
                && sameReturnTypes(methodDecl, m, ctx.reporter)) {
              /* The method is overriding */
              var methodSymbol = createMethodSymbol(methodDecl, classDecl)
              methodDecl.setSymbol(methodSymbol)
              methodDecl.id.setSymbol(methodDecl.getSymbol)
              classDecl.getSymbol.addMethod(methodDecl.id.value, methodDecl.getSymbol)
              methodDecl.retType match {
                case UntypedType() => methodDecl.getSymbol.setType(inferReturnType(methodDecl, classDecl))
                case _ => { }
              }
              for (param <- methodDecl.args) {
                val paramId = param.id.value
                glob.classes(classDecl.id.value).methods(methodDecl.id.value).lookupVar(paramId) match {
                  case Some(s) => printAlreadyDefined(paramId, param.id, ctx.reporter)
                  case None => {
                    createParameterSymbol(param)
                    methodDecl.getSymbol.addParam(paramId, param.getSymbol)
                    variables += param.getSymbol
                  }
                }
              }
            } else {
              ctx.reporter.error("Method does not override method with same name in superclass", methodDecl)
            }
          }
        }
        case None => {
          var methodSymbol = createMethodSymbol(methodDecl, classDecl)
          methodDecl.setSymbol(methodSymbol)
          methodDecl.id.setSymbol(methodDecl.getSymbol)
          classDecl.getSymbol.addMethod(methodDecl.id.value, methodDecl.getSymbol)
          for (param <- methodDecl.args) {
            val paramId = param.id.value
            glob.classes(classDecl.id.value).methods(methodDecl.id.value).lookupVar(paramId) match {
              case Some(s) => printAlreadyDefined(paramId, param.id, ctx.reporter)
              case None => {
                createParameterSymbol(param)
                methodDecl.getSymbol.addParam(paramId, param.getSymbol)
                variables += param.getSymbol
              }
            }
          }
        }
      }
    }

    /**
     * If the method already has been type inferred, we skip this method as it
     * would report "variable already declared" errors otherwise. Checks that
     * the variable has not been previously defined (in method).
     */
    def analyseMethodVariable(methodVar: VarDecl, method: MethodDecl, cd: ClassDecl): Unit = {
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
              /**
               * The declaration is also an assignment (var a = 7;), therefore we need
               * to handle the assignment as well.
               */
              attachIdentifier(methodVar.id)(method, cd)
              attachIdentifier(new Assign(methodVar.id, e))(method, cd)
              methodVar.getSymbol.setType(getTypeOfExprTree(e))
              variables += methodVar.getSymbol
              writtenVariables += methodVar.getSymbol
            }
            case None => variables += methodVar.getSymbol
          }
        }
      }
    }

    /**
     * The name speaks for itself.
     */
    def createParameterSymbol(formal: Formal): VariableSymbol = {
      var symbol = new VariableSymbol(formal.id.value)
      symbol.setPos(formal)
      symbol.setType(getTypeOfTypeTree(formal.tpe, ctx.reporter))
      formal.setSymbol(symbol)
      formal.id.setSymbol(formal.getSymbol)
      symbol
    }

    /**
     * Attach identifier to variables. That is, ensure that if we declare var a
     * in the method body, all consequent references to a should point to the
     * same symbol.
     */
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
                case Some(ms) => meth.setSymbol(ms)
                case None => { ctx.reporter.error("No method " + meth.value + " defined", meth) }
              }
            }
            case Identifier(value) => {
              glob.classes(obj.asInstanceOf[Identifier].getSymbol.getType.toString).lookupMethod(meth.value) match {
                case Some(ms) => meth.setSymbol(ms)
                case None => ctx.reporter.error("No method " + meth.value + " defined")
              }
            }
            case New(tpe) => {
              glob.classes(tpe.asInstanceOf[Identifier].getSymbol.getType.toString).lookupMethod(meth.value) match {
                case Some(ms) => meth.setSymbol(ms)
                case None => ctx.reporter.error("No method " + meth.value + " defined")
              }
            }
            case MethodCall(obj2, meth2, args2) => {
              glob.classes(getTypeOfExprTree(obj).toString).lookupMethod(meth.value) match {
                case Some(ms) => meth.setSymbol(ms)
                case None => ctx.reporter.error("No method " + meth.value + " defined")
              }
            }
            case _ => { sys.error(obj + " trying to invoke method " + meth.value) }
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
          id.getSymbol.getType match {
            case TUntyped => id.getSymbol.setType(getTypeOfExprTree(expr))
            case _ => { }
          }
          writtenVariables += id.getSymbol
        }
        case ArrayAssign(id, index, expr) => {
          attachIdentifier(id)
          attachIdentifier(index)
          attachIdentifier(expr)
          writtenVariables += id.getSymbol
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
          if (t.asInstanceOf[Identifier].hasSymbol) {
            readVariables += t.asInstanceOf[Identifier].getSymbol
          }
        }
        case IntLit(value) => { }
        case StringLit(value) => { }
        case True() => { }
        case False() => { }
        case _ => { sys.error("Trying to attach identifier to " + t) }
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
                // Must be fatal as otherwise, we will loop loop loop
                ctx.reporter.fatal("cycles in the inheritance graph", p)
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
            variables += classVar.getSymbol
          }
        }
      }

      /* Iterate through methods of the class */
      classDecl.methods foreach { m => analyseMethod(m, classDecl) }
    }

    /* Iterate through all variables (class, method, parameters) */
    for (classDecl <- prog.classes :+ mainClassDecl) {
      for (method <- classDecl.methods) {
        for (param <- method.args) {
          param.tpe match {
            case Identifier(_) => {
              attachIdentifier(param.tpe.asInstanceOf[Identifier])(method, classDecl)
            }
            case _ => { }
          }
        }
        /*method.retType match {
          case UntypedType() => method.id.getSymbol.setType(inferReturnType(method, classDecl))
          case _ => method.id.getSymbol.setType(getTypeOfTypeTree(method.retType, ctx.reporter))
        }*/
        for (classVar <- classDecl.vars) {
          variables += classVar.getSymbol
          classVar.tpe match {
            case Identifier(value) => {
              // Class type
              attachIdentifier(classVar.tpe.asInstanceOf[Identifier])(method, classDecl)
            }
            case _ => { /* Primitive type */ }
          }
        }
        //method.vars foreach { mv => analyseMethodVariable(mv, method, classDecl)}

        classDecl.parent match {
          case Some(p) => {
            attachIdentifier(p)(method, classDecl)
          }
          case None => { }
        }

        method.id.getSymbol.setType(inferReturnType(method, classDecl))

        typeInferredMethods += method.getSymbol
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
      }
    }

    // Step 2: Attach symbols to identifiers (except method calls) in method bodies
    // (Step 3:) Print tree with symbol ids for debugging

    // Make sure you check all constraints

    for (variable <- variables) {
      if (!(readVariables contains variable)) {
        if (!(writtenVariables contains variable)) {
          ctx.reporter.warning(s"Variable ${variable.name} is not used", variable)
        } else {
          ctx.reporter.warning(s"Variable ${variable.name} is read but never written", variable)
        }
      } else if (!(writtenVariables contains variable)) {
        ctx.reporter.warning(s"Variable ${variable.name} is written but never read", variable)
      }
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
