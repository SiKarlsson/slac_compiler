package slacc
package analyzer

import ast.Trees._

import Symbols._
import Types._
import utils._
import NameAnalysis.getTypeOfTypeTree

object TypeChecking extends Pipeline[Program, Program] {

  // Current class we're operating in
  var classSymbolScope: Option[ClassSymbol] = None

  /** Typechecking does not produce a value, but has the side effect of
   * attaching types to trees and potentially outputting error messages. */
  def run(ctx: Context)(prog: Program): Program = {
    import ctx.reporter._

    def typeCheckExprs(exprs: List[ExprTree]): Unit = {
      exprs foreach { e => tcExpr(e) }
    }

    def typeCheckVarDecls(varDecls: List[VarDecl]): Unit = {
      for (variable <- varDecls) {
        variable.expr match {
          case Some(e) => {
            val varType = tcExpr(e)
            variable.getSymbol.setType(varType)
          }
          case None => { }
        }
      }
    }

    // Type check each expression of the main body
    typeCheckExprs(prog.main.main.exprs)
    typeCheckVarDecls(prog.main.main.vars)
    // Ensure that main returns Unit
    tcExpr(prog.main.main.retExpr, TUnit)

    for (classDecl <- prog.classes) {
      classSymbolScope = Some(classDecl.getSymbol)
      for (methodDecl <- classDecl.methods) {
        typeCheckVarDecls(methodDecl.vars)
        typeCheckExprs(methodDecl.exprs)
        var rt = getTypeOfTypeTree(methodDecl.retType, ctx.reporter)
        rt match {
          case TUntyped => {
            /* If rt is TUntyped, we don't know the return type of the method
            and have to parse the return expression of the method to find out */
            rt = tcExpr(methodDecl.retExpr)
          }
          case _ => { }
        }
        methodDecl.id.getSymbol.setType(rt)
        methodDecl.getSymbol.setType(rt)
        tcExpr(methodDecl.retExpr, rt)
      }
    }

    def tcExpr(expr: ExprTree, expected: Type*): Type = {
      val tpe: Type = expr match {
        case And(lhs, rhs) => {
          tcExpr(lhs, TBoolean)
          tcExpr(rhs, TBoolean)
          TBoolean
        }
        case Or(lhs, rhs) => {
          tcExpr(lhs, TBoolean)
          tcExpr(rhs, TBoolean)
          TBoolean
        }
        case Plus(lhs: ExprTree, rhs: ExprTree) => {
          val lhsT = tcExpr(lhs, TInt, TString)

          lhsT match {
            case TInt => {
              tcExpr(rhs, TInt, TString)
            }
            case TString => {
              tcExpr(rhs, TInt, TString)
              TString
            }
            case _ => sys.error(s"${rhs} is not String or Int (Can't use addition)")
          }
        }
        case Minus(lhs: ExprTree, rhs: ExprTree) => {
          tcExpr(lhs, TInt)
          tcExpr(rhs, TInt)
          TInt
        }
        case Times(lhs: ExprTree, rhs: ExprTree) => {
          tcExpr(lhs, TInt)
          tcExpr(rhs, TInt)
          TInt
        }
        case Div(lhs: ExprTree, rhs: ExprTree) => {
          tcExpr(lhs, TInt)
          tcExpr(rhs, TInt)
          TInt
        }
        case LessThan(lhs: ExprTree, rhs: ExprTree) => {
          tcExpr(lhs, TInt)
          tcExpr(rhs, TInt)
          TBoolean
        }
        case Equals(lhs: ExprTree, rhs: ExprTree) => {
          tcExpr(lhs, TInt, TString, TIntArray, TBoolean, TUnit, Types.anyObject) match {
            case TInt => { tcExpr(rhs, TInt) }
            case TBoolean => { tcExpr(rhs, TBoolean) }
            case TIntArray => { tcExpr(rhs, TIntArray) }
            case TString => { tcExpr(rhs, TString) }
            case TUnit => { tcExpr(rhs, TUnit) }
            case TClass(_) => { tcExpr(rhs, Types.anyObject) }
            case TUntyped => { ctx.reporter.error(s"${lhs} has no type yet, has it been initialized?") }
            case _ => {
              sys.error("Tried to match something unexpected in an equals expression")
            }
          }
          TBoolean
        }
        case ArrayRead(arr: ExprTree, index: ExprTree) => {
          tcExpr(arr, TIntArray)
          tcExpr(index, TInt)
          TInt
        }
        case ArrayLength(arr: ExprTree) => {
          tcExpr(arr, TIntArray)
          TInt
        }
        case MethodCall(obj: ExprTree, meth: Identifier, args: List[ExprTree]) => {
          val c = tcExpr(obj)
          if (!c.isInstanceOf[TClass]) {
            ctx.reporter.error("Can't invoke method from ", obj)
            return TUntyped
          }
          val cs = c.asInstanceOf[TClass].getClassSymbol
          val ms = cs.lookupMethod(meth.value)
          val retType: Type = (ms match {
            case Some(m) => {
              m.asInstanceOf[MethodSymbol].overridden match {
                case Some(om) => meth.asInstanceOf[Identifier].setSymbol(om)
                case None => meth.asInstanceOf[Identifier].setSymbol(m)
              }
              if (m.argList.size == args.size) {
                // Ensure that the provided arguments have the same types as the
                // corresponding parameter
                for ( (mArg, mParam) <- (args zip m.argList)) yield tcExpr(mArg, mParam.getType)
              } else {
                ctx.reporter.error("Wrong amount of arguments to method", obj)
              }
              m.getType match {
                case TUntyped => ctx.reporter.error("Cannot infer type", m)
                case _ => { }
              }

              m.getType
            }
            case None => {
              ctx.reporter.error("Method does not belong to this class", obj)
              TUntyped
            }
          })
          retType
        }
        case IntLit(value) => TInt
        case StringLit(value) => TString
        case True() => TBoolean
        case False() => TBoolean
        case Identifier(id) => expr.getType
        case Self() => {
          classSymbolScope match {
            case Some(cs) => {
              expr.asInstanceOf[Self].setSymbol(cs)
              TClass(cs)
            }
            case None => sys.error("There is no scope for this Self()?")
          }
        }
        case NewIntArray(size: ExprTree) => {
          tcExpr(size, TInt)
          TIntArray
        }
        case New(tpe: Identifier) => {
          TClass(NameAnalysis.glob.classes(tpe.value))
        }
        case Not(expr: ExprTree) => {
          tcExpr(expr, TBoolean)
          TBoolean
        }
        case Block(exprs: List[ExprTree]) => {
          var lastType: Option[Type] = None
          for (expr <- exprs.dropRight(1)) {
            tcExpr(expr)
          }
          lastType = Some(tcExpr(exprs.last))
          lastType match {
            case Some(lt) => lt
            case None => { sys.error(s"${expr}'s last expression does not have a type") }
          }
        }
        case If(expr: ExprTree, thn: ExprTree, els: Option[ExprTree]) => {
          tcExpr(expr, TBoolean)
          val thenType = tcExpr(thn)
          els match {
            case Some(e) => {
              val elsType = tcExpr(e)
              if (elsType != thenType) {
                ctx.reporter.error("Bodies of then and else do not return the same type", expr)
              }
            }
            case None => { }
          }
          thenType
        }
        case While(cond: ExprTree, body: ExprTree) => {
          tcExpr(cond, TBoolean)
          tcExpr(body, TUnit)
          TUnit
        }
        case Println(expr: ExprTree) => {
          tcExpr(expr, TString)
          TUnit
        }
        case Assign(id: Identifier, expr: ExprTree) => {
          val idType = id.getSymbol.getType
          idType match {
            case TUntyped => {
              id.getSymbol.setType(tcExpr(expr))
            }
            case _ => tcExpr(expr, idType)
          }
          TUnit
        }
        case ArrayAssign(id: Identifier, index: ExprTree, expr: ExprTree) => {
          tcExpr(id, TIntArray)
          tcExpr(index, TInt)
          tcExpr(expr, TInt)
          TUnit
        }
        case Strof(expr: ExprTree) => {
          tcExpr(expr, TInt, TBoolean)
          TString
        }
        case _ => { sys.error("No typechecking for " + expr)}
      }
      expr.setType(tpe)

      // Check result and return a valid type in case of error
      if (expected.isEmpty) {
        tpe
      } else if (!expected.exists(e => tpe.isSubTypeOf(e))) {
        error("Type error: expected: " + expected.toList.mkString(" or ") + ", found: " + tpe, expr)
        expected.head
      } else {
        tpe
      }
    }

    prog
  }

}
