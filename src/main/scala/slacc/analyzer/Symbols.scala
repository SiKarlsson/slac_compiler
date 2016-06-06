package slacc
package analyzer

import utils._
import Types._
import ast.Trees._

object Symbols {
  trait Symbolic[S <: Symbol] {
    private var _sym: Option[S] = None

    def setSymbol(sym: S): this.type = {
      _sym = Some(sym)
      this
    }

    def getSymbol: S = _sym match {
      case Some(s) => s
      case None => sys.error("Accessing undefined symbol.")
    }

    def hasSymbol: Boolean = _sym match {
      case Some(s) => true
      case None => false
    }
  }

  sealed abstract class Symbol extends Positioned with Typed {
    val id: Int = ID.next
    val name: String
  }

  private object ID {
    private var c: Int = 0

    def next: Int = {
      val ret = c
      c = c + 1
      ret
    }
  }

  class GlobalScope {
    var mainClass: ClassSymbol = _
    var classes = Map[String, ClassSymbol]()

    def lookupClass(n: String): Option[ClassSymbol] = classes get n

    def addClass(n: String, cs: ClassSymbol): Unit = {
      classes = classes + (n -> cs)
    }
  }

  class ClassSymbol(val name: String) extends Symbol {
    var parent: Option[ClassSymbol] = None
    var methods = Map[String, MethodSymbol]()
    var members = Map[String, VariableSymbol]()
    var declaration: Option[ClassDecl] = None

    def getDeclaration(): Option[ClassDecl] = declaration
    def setDeclaration(c: ClassDecl) = { declaration = Some(c) }

    def lookupMethod(n: String): Option[MethodSymbol] = {
      methods get n match {
        case Some(m) => Some(m)
        case None => {
          parent match {
            case Some(p) => p.lookupMethod(n)
            case None => None
          }
        }
      }
    }

    def lookupVar(n: String): Option[VariableSymbol] = {
      members get n match {
        case Some(m) => Some(m)
        case None => {
          parent match {
            case Some(p) => { p.lookupVar(n) }
            case None => None
          }
        }
      }
    }

    def addMember(n: String, vs: VariableSymbol): Unit = {
      members = members + (n -> vs)
    }

    def addMethod(n: String, ms: MethodSymbol): Unit = {
      methods = methods + (n -> ms)
    }
  }

  class MethodSymbol(val name: String, val classSymbol: ClassSymbol) extends Symbol {
    var params = Map[String, VariableSymbol]()
    var members = Map[String, VariableSymbol]()
    var argList: List[VariableSymbol] = Nil
    var overridden: Option[MethodSymbol] = None
    var declaration: Option[MethodDecl] = None

    def getDeclaration(): Option[MethodDecl] = declaration
    def setDeclaration(m: MethodDecl) = { declaration = Some(m) }

    def lookupVar(n: String): Option[VariableSymbol] = members get n match {
      case Some(e) => { Some(e) }
      case None => { params get n }
    }

    def addParam(n: String, vs: VariableSymbol): Unit = {
      params = params + (n -> vs)
      argList = argList :+ vs
    }

    def addMember(n: String, vs: VariableSymbol): Unit = {
      members = members + (n -> vs)
    }
  }

  class VariableSymbol(val name: String) extends Symbol
}
