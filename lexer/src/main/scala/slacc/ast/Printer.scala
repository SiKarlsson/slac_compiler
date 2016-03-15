package slacc
package ast

import Trees._

object Printer {
  def apply(t: Tree): String = {
      t match {
          case Program(mainMethod, classes) => {
              var programString = ""
              for (classDecl <- classes) {
                programString += apply(classDecl) + "\n"
              }
              programString += apply(mainMethod)
              return programString
          }
          case MainMethod(methodDecl) => {
              return apply(methodDecl)
          }
          case ClassDecl(id, parent, vars, methods) => {
            var classString = "class ".concat(apply(id))
            if (parent.isEmpty) {
              classString += " {" + "\n"
            } else {
              classString += " <: ".concat(apply(parent.get)).concat(" {") + "\n"
            }
            for (vari <- vars) {
              classString += apply(vari) + "\n"
            }
            for (method <- methods) {
              classString += apply(method) + "\n"
            }
            classString += "}"
            return classString
          }
          case VarDecl(tpe, id) => {
            return "var ".concat(apply(id)).concat(" : ").concat(apply(tpe)).concat(";")
          }
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
            methodString += ") : " + apply(retType) + " {\n"
            for (vari <- vars) {
              methodString += apply(vari) + "\n"
            }
            for (expr <- exprs) {
              methodString += apply(expr) + "\n"
            }
            methodString += apply(retExpr) + "\n}"
            return methodString
          }
          case IntArrayType() => {
            return "Int[]"
          }
          case IntType() => {
            return "Int"
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
          case And(lhs, rhs) => {
            return "(".concat(apply(lhs).concat(" && ").concat(apply(rhs))).concat(")")
          }
          case Or(lhs, rhs) => {
            return "(".concat(apply(lhs).concat(" || ").concat(apply(rhs))).concat(")")
          }
          case Plus(lhs, rhs) => {
            return "(".concat(apply(lhs).concat(" + ").concat(apply(rhs))).concat(")")
          }
          case Minus(lhs, rhs) => {
            return "(".concat(apply(lhs).concat(" - ").concat(apply(rhs))).concat(")")
          }
          case Times(lhs, rhs) => {
            return "(".concat(apply(lhs).concat(" * ").concat(apply(rhs))).concat(")")
          }
          case Div(lhs, rhs) => {
            return "(".concat(apply(lhs).concat(" / ").concat(apply(rhs))).concat(")")
          }
          case LessThan(lhs, rhs) => {
            return "(".concat(apply(lhs).concat(" < ").concat(apply(rhs))).concat(")")
          }
          case Equals(lhs, rhs) => {
            return "(".concat(apply(lhs).concat(" == ").concat(apply(rhs))).concat(")")
          }
          case ArrayRead(arr, index) => {
            return apply(arr).concat("[").concat(apply(index)).concat("]")
          }
          case ArrayLength(arr) => {
            return apply(arr).concat(".length")
          }
          case MethodCall(obj, meth, args) => {
            var methodCall = apply(obj).concat(".").concat(apply(meth)).concat("(")
            var first = true
              for (arg <- args) {
                if (!first) {
                  methodCall += ", "
                } else {
                  first = false
                }
                methodCall += apply(arg)
              }
            methodCall += ")"
            return methodCall
          }
          case IntLit(value) => {
            return value.toString
          }
          case StringLit(value) => {
            return value
          }
          case True() => {
            return "true"
          }
          case False() => {
            return "false"
          }
          case Identifier(value) => {
            return value
          }
          case Self() => {
            return "self"
          }
          case NewIntArray(size) => {
            return "new Int[".concat(apply(size)).concat("]")
          }
          case New(tpe) => {
            return "new ".concat(apply(tpe)).concat("()")
          }
          case Not(expr) => {
            return "!".concat(apply(expr))
          }
          case Block(exprs) => {
            var blockString = "{\n"
            for (expr <- exprs) {
              blockString += apply(expr) + "\n"
            }
            blockString += "}"
            return blockString
          }
          case If(expr, thn, els) => {
            var ifString = "if (".concat(apply(expr)).concat(") ")
            ifString += apply(thn)
            if (!els.isEmpty) {
              ifString += " else ".concat(apply(els.get))
            }
            return ifString
          }
          case While(cond, body) => {
            return "while (".concat(apply(cond)).concat(") ").concat(apply(body))
          }
          case Println(expr) => {
            var printString = "Println(\""
            printString = printString.concat(apply(expr))
            printString = printString.concat("\")")
            return printString
          }
          case Assign(id, expr) => {
            return apply(id).concat(" = ").concat(apply(expr))
          }
          case ArrayAssign(id, index, expr) => {
            return apply(id).concat("[").concat(apply(index)).concat("]")
              .concat(" = ").concat(apply(expr))
          }
          case Formal(varType, id) => {
            return apply(id) + ": " + apply(varType)
          }
          case Strof(expr) => {
            return "strOf(".concat(apply(expr)).concat(")")
          }
          case _ => {
              ":("
          }
      }
  }
}
