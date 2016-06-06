package slacc
package ast

import Trees._

object Printer {
  var tabCount = 0
  //var symid = true;
  def apply(t: Tree)(implicit symid: Boolean): String = {
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
            apply(methodDecl)
          }
          case ClassDecl(id, parent, vars, methods) => {
            var classString = "class ".concat(apply(id))
            if (parent.isEmpty) {
              classString += " {" + "\n"
            } else {
              classString += " <: ".concat(apply(parent.get)).concat(" {") + "\n"
            }
            incrementTabCount
            for (vari <- vars) {
              classString += printTabs.concat(apply(vari)) + "\n"
            }
            for (method <- methods) {
              classString += printTabs.concat(apply(method)) + "\n"
            }
            decrementTabCount
            classString += printTabs.concat("}")
            return classString
          }
          case VarDecl(tpe, id, expr) => {
            var varString = "var ".concat(apply(id))
            //if (symid) varString = varString.concat("#").concat(t.asInstanceOf[VarDecl].getSymbol.id.toString)
            varString = varString.concat(" : ").concat(apply(tpe)).concat(";");
            return varString
          }
          case MethodDecl(retType, id, args, vars, exprs, retExpr) => {
            var methodString = "method ".concat(apply(id))
            methodString = methodString.concat("(")
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
            incrementTabCount
            for (vari <- vars) {
              methodString += printTabs.concat(apply(vari)) + "\n"
            }
            for (expr <- exprs) {
              methodString += printTabs.concat(apply(expr)) + ";\n"
            }
            methodString += printTabs.concat(apply(retExpr)) + "\n"
            decrementTabCount
            methodString += printTabs.concat("}")
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
            return "\"".concat(value).concat("\"")
          }
          case True() => {
            return "true"
          }
          case False() => {
            return "false"
          }
          case Identifier(value) => {
            var s = value
            if (symid) {
              if (t.asInstanceOf[Identifier].hasSymbol) {
                s = s.concat("#").concat(t.asInstanceOf[Identifier].getSymbol.id.toString).concat("(: ").concat(t.asInstanceOf[Identifier].getSymbol.getType.toString).concat(")")
              } else {
                s = s.concat("#??")
              }
            }
            s
          }
          case Self() => {
            var s = "self"
            if (symid) {
              if (t.asInstanceOf[Self].hasSymbol) {
                s = s.concat("#").concat(t.asInstanceOf[Self].getSymbol.id.toString)
              } else {
                s = s.concat("#??")
              }
            }
            s
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
            incrementTabCount
            for (expr <- exprs) {
              blockString += printTabs.concat(apply(expr)) + "\n"
            }
            decrementTabCount
            blockString += printTabs.concat("}")
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
            var printString = "Println("
            printString = printString.concat(apply(expr))
            printString = printString.concat(")")
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
            var s = apply(id) + ""
            //if (symid) s = s.concat("#").concat(t.asInstanceOf[Formal].getSymbol.id.toString)
            s = s.concat(": " + apply(varType))
            s
          }
          case Strof(expr) => {
            return "strOf(".concat(apply(expr)).concat(")")
          }
          case _ => {
              ":("
          }
      }
  }

  def printTabs(): String = {
    var tabs = ""
    for (i <- 1 to tabCount){
      tabs += "  "
    }
    tabs
  }

  def incrementTabCount(): Unit = {
    tabCount = tabCount + 1
  }

  def decrementTabCount(): Unit = {
    tabCount = tabCount - 1
  }
}
