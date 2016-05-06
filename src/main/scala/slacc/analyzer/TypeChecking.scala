package slacc
package analyzer

import ast.Trees._

import Symbols._
import Types._
import utils._
import NameAnalysis.getTypeOfTypeTree

object TypeChecking extends Pipeline[Program, Program] {

  /** Typechecking does not produce a value, but has the side effect of
   * attaching types to trees and potentially outputting error messages. */
  def run(ctx: Context)(prog: Program): Program = {
    import ctx.reporter._

    for (classDecl <- prog.classes) {
      for (methodDecl <- classDecl.methods) {
        for (expr <- methodDecl.exprs) {
          tcExpr(expr)
        }
        tcExpr(methodDecl.retExpr, getTypeOfTypeTree(methodDecl.retType))
      }
    }

    def tcExpr(expr: ExprTree, expected: Type*): Type = {
      val tpe: Type = expr match {
        case And(lhs, rhs) => {
          tcExpr(lhs, TBoolean, TInt)
          tcExpr(rhs, TBoolean)
        }
        case Or(lhs, rhs) => {
          tcExpr(lhs, TBoolean)
          tcExpr(rhs, TBoolean)
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
              }
            }
            case TString => {
              TString
            }
          }
        }
        case Minus(lhs: ExprTree, rhs: ExprTree) => {
          tcExpr(lhs, TInt)
          tcExpr(rhs, TInt)
        }
        case Times(lhs: ExprTree, rhs: ExprTree) => {
          tcExpr(lhs, TInt)
          tcExpr(rhs, TInt)
        }
        case Div(lhs: ExprTree, rhs: ExprTree) => {
          tcExpr(lhs, TInt)
          tcExpr(rhs, TInt)
        }
        case LessThan(lhs: ExprTree, rhs: ExprTree) => {
          tcExpr(lhs, TInt)
          tcExpr(rhs, TInt)
        }
        case Equals(lhs: ExprTree, rhs: ExprTree) => {
          tcExpr(lhs) match {
            case TInt => {
                tcExpr(rhs, TInt)
            }
            case TString => {
              tcExpr(rhs, TString)
            }
            case TIntArray => {
              tcExpr(rhs, TIntArray)
            }
          }
        }
        case ArrayRead(arr: ExprTree, index: ExprTree) => {
          tcExpr(arr, TIntArray)
          tcExpr(index, TInt)
        }
        case ArrayLength(arr: ExprTree) => {
          tcExpr(arr, TIntArray)
        }
        case MethodCall(obj: ExprTree, meth: Identifier, args: List[ExprTree]) => ???
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
        case Self() => ???
        case NewIntArray(size: ExprTree) => {
          tcExpr(size, TInt)
        }
        case New(tpe: Identifier) => ???
        case Not(expr: ExprTree) => {
          tcExpr(expr, TBoolean)
        }
        case Block(exprs: List[ExprTree]) => ???
        case If(expr: ExprTree, thn: ExprTree, els: Option[ExprTree]) => ???
        case While(cond: ExprTree, body: ExprTree) => ???
        case Println(expr: ExprTree) => ???
        case Assign(id: Identifier, expr: ExprTree) => ???
        case ArrayAssign(id: Identifier, index: ExprTree, expr: ExprTree) => ???
        case Strof(expr: ExprTree) => ???
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
