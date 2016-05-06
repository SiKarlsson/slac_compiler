package slacc
package analyzer

import ast.Trees._

import Symbols._
import Types._
import utils._

object TypeChecking extends Pipeline[Program, Program] {

  /** Typechecking does not produce a value, but has the side effect of
   * attaching types to trees and potentially outputting error messages. */
  def run(ctx: Context)(prog: Program): Program = {
    import ctx.reporter._

    def tcExpr(expr: ExprTree, expected: Type*): Type = {
      val tpe: Type = expr match {
        case And(lhs, rhs) => {
          tcExpr(lhs, TBoolean)
          tcExpr(rhs, TBoolean)
        }
        case Plus(lhs: ExprTree, rhs: ExprTree) => ???
        case Minus(lhs: ExprTree, rhs: ExprTree) => ???
        case Times(lhs: ExprTree, rhs: ExprTree) => {
          tcExpr(lhs, TInt)
          tcExpr(rhs, TInt)
        }
        case Div(lhs: ExprTree, rhs: ExprTree) => {
          tcExpr(lhs, TInt)
          tcExpr(rhs, TInt)
        }
        case LessThan(lhs: ExprTree, rhs: ExprTree) => ???
        case Equals(lhs: ExprTree, rhs: ExprTree) => ???
        case ArrayRead(arr: ExprTree, index: ExprTree) => {
          tcExpr(arr, TIntArray)
          tcExpr(index, TInt)
        }
        case ArrayLength(arr: ExprTree) => {
          tcExpr(arr, TIntArray)
        }
        case MethodCall(obj: ExprTree, meth: Identifier, args: List[ExprTree]) => ???
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
