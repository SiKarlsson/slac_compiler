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
        tcExpr(methodDecl.retExpr, getTypeOfTypeTree(methodDecl.retType))
      }
    }

    def tcExpr(expr: ExprTree, expected: Type*): Type = {
      val tpe: Type = expr match {
        case And(lhs, rhs) => {
          tcExpr(lhs, TBoolean)
          tcExpr(rhs, TBoolean)
        }
        case Or(lhs, rhs) => {
          tcExpr(lhs, TBoolean)
          tcExpr(rhs, TBoolean)
        }
        case True() => {
          TBoolean
        }
        case False() => {
          TBoolean
        }
        case IntLit() => {
          TInt
        }
        case StrLit() => {
          TString
        }
        case Identifier(id) => {
          expr.getType
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
