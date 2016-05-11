package slacc
package analyzer

import ast.Trees._

import Symbols._
import Types._
import utils._
import NameAnalysis.getTypeOfTypeTree

object TypeChecking extends Pipeline[Program, Program] {

  var classSymbolScope: Option[ClassSymbol] = None

  /** Typechecking does not produce a value, but has the side effect of
   * attaching types to trees and potentially outputting error messages. */
  def run(ctx: Context)(prog: Program): Program = {
    import ctx.reporter._

    tcExpr(prog.main.main.retExpr, TUnit)

    for (classDecl <- prog.classes) {
      classSymbolScope = Some(classDecl.getSymbol)
      for (methodDecl <- classDecl.methods) {
        for (expr <- methodDecl.exprs) {
          tcExpr(expr)
        }
        tcExpr(methodDecl.retExpr, getTypeOfTypeTree(methodDecl.retType, ctx.reporter))
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
          val rhsT = tcExpr(rhs, TInt, TString)

          lhsT match {
            case TInt => {
              rhsT match {
                case TInt => {
                  TInt
                }
                case TString => {
                  TString
                }
                case _ => {
                  sys.error("Tried to match something else than TInt or TString in a plus expression")
                }
              }
            }
            case TString => {
              tcExpr(rhs, TInt, TString)
              TString
            }
            case _ => {
              sys.error("Tried to match something else than TInt or TString in a plus expression")
            }
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
          tcExpr(lhs, TInt, TString, TIntArray, TBoolean, TUnit) match {
            case TInt => { tcExpr(rhs, TInt) }
            case TBoolean => { tcExpr(rhs, TBoolean) }
            case TIntArray => { tcExpr(rhs, TIntArray) }
            case TString => { tcExpr(rhs, TString) }
            case TUnit => { tcExpr(rhs, TUnit) }
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
          val cs = c.asInstanceOf[TClass].getClassSymbol
          val ms = cs.lookupMethod(meth.value)
          var retType: Option[Type] = None
          ms match {
            case Some(m) => {
              meth.asInstanceOf[Identifier].setSymbol(m)
              if (m.argList.size == args.size) {
                for (classDecl <- prog.classes) {
                  if (classDecl.id.value == cs.getType.toString) {
                    for (methodDecl <- classDecl.methods) {
                      if (methodDecl.id.value == meth.value) {
                        retType = Some(NameAnalysis.getTypeOfTypeTree(methodDecl.retType, ctx.reporter))
                      }
                    }
                  }
                }
                for (arg <- args) {
                  tcExpr(arg)
                }
              } else {
                ctx.reporter.error("Wrong amount of arguments to method", obj)
              }
            }
            case None => ctx.reporter.error("Method does not belong to this class", obj)
          }

          // TODO: Hur ska vi hitta metodens returtyp?
          retType match {
            case Some(rt) => {
              meth.asInstanceOf[Identifier].getSymbol.setType(rt)
              rt
            }
            case None => sys.error("No return type for method " + meth.value)
          }
        }
        case IntLit(value) => {
          TInt
        }
        case StringLit(value) => {
          TString
        }
        case True() => {
          TBoolean
        }
        case False() => {
          TBoolean
        }
        case Identifier(id) => {
          expr.getType
        }
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
          var lastType: Type = TUnit
          for (expr <- exprs) {
            lastType = tcExpr(expr)
          }
          lastType
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
          tcExpr(expr, idType)
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
