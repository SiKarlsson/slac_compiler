package slacc
package ast

import Trees._

object Printer {
  def apply(t: Tree): String = {
      t match {
          case Program(mainMethod, classes) => {
              return apply(mainMethod)
          }
          case MainMethod(methodDecl) => {
              return apply(methodDecl)
          }
          /* (retType: TypeTree, id: Identifier, args: List[Formal],
          vars: List[VarDecl], exprs: List[ExprTree], retExpr: ExprTree)
          extends Tree {
          }*/
          case MethodDecl(retType, id, args, vars, exprs, retExpr) => {
              var methodString = "method ".concat(apply(id)).concat("(")
              var first = true
              for (arg <- args) {
                if (!first) {
                  methodString += ", "
                } else {
                  first = false
                }
                methodString += apply(arg)
              }
              methodString += ") : " + apply(retType) + " { \n"
              for (expr <- exprs) {
                methodString += apply(expr)
              }
              methodString += apply(retExpr)
              methodString += "\n}"
              return methodString
          }
          case Println(expr) => {
            var printString = "\tPrintln(\""
            printString = printString.concat(apply(expr))
            printString = printString.concat("\")")
            return printString
          }
          case VarDecl(varType, id) => {
            return apply(id) + ": " + apply(varType)
          }
          case Formal(varType, id) => {
            return apply(id) + ": " + apply(varType)
          }
          case IntType() => {
            return "Int"
          }
          case IntArrayType() => {
            return "Int[]"
          }
          case BooleanType() => {
            return "Boolean"
          }
          case StringType() => {
            return "String"
          }
          case UnitType() => {
              return "Unit"
          }
          case Identifier(value) => {
              return value
          }
          case StringLit(value) => {
            return value
          }
          case _ => {
              ":("
          }
      }
  }
}
