package slacc
package ast

import utils._
import Trees._
import lexer._
import lexer.Tokens._
import scala.collection.mutable.ListBuffer

object Parser extends Pipeline[Iterator[Token], Program] {
  def run(ctx: Context)(tokens: Iterator[Token]): Program = {
    import ctx.reporter._

    /** Store the current token, as read from the lexer. */
    var currentToken: Token = new Token(BAD)

    def readToken: Unit = {
      if (tokens.hasNext) {
        // uses nextToken from the Lexer trait
        currentToken = tokens.next

        // skips bad tokens
        while (currentToken.kind == BAD) {
          currentToken = tokens.next
        }
      }
    }

    /** ''Eats'' the expected token, or terminates with an error. */
    def eat(kind: TokenKind): Unit = {
      println("eating " + currentToken.kind + ", expecting " + kind)
      if (currentToken.kind == kind) {
        readToken
      } else {
        expected(kind)
      }
    }

    /** Complains that what was found was not expected. The method accepts arbitrarily many arguments of type TokenKind */
    def expected(kind: TokenKind, more: TokenKind*): Nothing = {
      fatal("expected: " + (kind::more.toList).mkString(" or ") + ", found: " + currentToken, currentToken)
    }

    def parseGoal: Program = {

      val classDeclList = new ListBuffer[ClassDecl]()
      var mainMethod: Option[MainMethod] = None

      while (currentToken.kind == CLASS) {
        classDeclList += classDeclaration
      }

      if (currentToken.kind == METHOD) {
        mainMethod = Some(new MainMethod(methodDeclaration))
      }

      def classDeclaration = {
        // class Identifier ( <: Identifier )? { ( VarDeclaration )* ( MethodDeclaration )* }
        readToken
        val ident = identifier
        var parent: Option[Identifier] = None
        if (currentToken.kind == LESSTHAN) {
          readToken
          eat(COLON)
          parent = Some(identifier)
        }
        eat(LBRACE)
        val vars = new ListBuffer[VarDecl]()
        while (currentToken.kind == VAR) {
          vars += varDeclaration
        }
        val methods = new ListBuffer[MethodDecl]()
        while (currentToken.kind == METHOD) {
          methods += methodDeclaration
        }
        eat(RBRACE)
        new ClassDecl(ident, parent, vars.toList, methods.toList)
      }

      def varDeclaration: VarDecl = {
        // var Identifier : Type ;
        eat(VAR)
        val ident = identifier
        eat(COLON)
        val tt = typeTree
        eat(SEMICOLON)
        new VarDecl(tt, ident)
      }

      def methodDeclaration = {
        /* method Identifier ( ( Identifier : Type ( , Identifier : Type )* )? )
         : Type = { ( VarDeclaration )* Expression ( ; Expression )* } */
        eat(METHOD)
        val ident = identifier
        val argsList = new ListBuffer[Formal]()
        val varList = new ListBuffer[VarDecl]()
        val exprList = new ListBuffer[ExprTree]()
        eat(LPAREN)
        if (currentToken.kind == IDKIND) {
          var argIdent = identifier
          eat(COLON)
          var argType = typeTree
          argsList :+ new Formal(argType, argIdent)
          while (currentToken.kind == COMMA) {
            readToken
            argIdent = identifier
            eat(COLON)
            argType = typeTree
            argsList :+ new Formal(argType, argIdent)
          }
        }
        eat(RPAREN)
        eat(COLON)
        val retType = typeTree
        eat(EQSIGN)
        eat(LBRACE)
        while (currentToken.kind == VAR) {
          varList :+ varDeclaration
        }
        exprList += expression
        while (currentToken.kind == SEMICOLON) {
          exprList :+ expression
        }
        eat(RBRACE)

        new MethodDecl(retType, ident, argsList.toList, varList.toList,
          exprList.toList, exprList.toList.last)
      }

      def typeTree = {
        if (currentToken.kind == INTLITKIND) {
          readToken
          if (currentToken.kind == LBRACKET) {
            eat(RBRACKET)
            new IntArrayType
          } else {
            new IntType
          }
        } else if (currentToken.kind == BOOLEAN) {
          readToken
          new BooleanType
        } else if (currentToken.kind == STRLITKIND) {
          readToken
          new StringType
        } else if (currentToken.kind == UNIT) {
          readToken
          new UnitType
        } else {
          identifier
        }
      }

      def expression: ExprTree = {
        currentToken.kind match {
          case TRUE => {
            readToken
            return new True()
          }
          case FALSE => {
            readToken
            return new False()
          }
          case SELF => {
            readToken
            return new Self()
          }
          case INTLITKIND => {
            val value = currentToken.asInstanceOf[INTLIT].value
            readToken
            return new IntLit(value)
          }
          case STRLITKIND => {
            val value = currentToken.asInstanceOf[STRLIT].value
            readToken
            return new StringLit(value)
          }
          case IDKIND => return identifier
          case NEW => {
            readToken
            if (currentToken.kind == INT) {
              readToken
              eat(LBRACKET)
              val expr = expression
              eat(RBRACKET)
              return new NewIntArray(expr)
            } else {
              val ident = identifier
              eat(LPAREN)
              eat(RPAREN)
              return new New(ident)
            }
          }
          case BANG => {
            readToken
            return new Not(expression)
          }
          case LPAREN => {
            eat(LPAREN)
            expression
            eat(RPAREN)
          }
          case LBRACE => {
            readToken
            val block = List()
            block :+ expression
            while (currentToken.kind == SEMICOLON) {
              block :+ expression
            }
            eat(LBRACE)
          }
          case IF => {
            readToken
            eat(LPAREN)
            val cond = expression
            eat(RPAREN)
            val thn = expression
            var els: Option[ExprTree] = None
            if (currentToken.kind == ELSE) {
              readToken
              els = Some(expression)
            }
            new If(cond, thn, els)
          }
          case WHILE => {
            eat(LPAREN)
            readToken
            val cond = expression
            eat(RPAREN)
            val body = expression
            new While(cond, body)
          }
          case PRINTLN => {
            readToken
            eat(LPAREN)
            val expr = expression
            eat(RPAREN)
            new Println(expr)
          }
          case STROF => {
            readToken
            eat(LPAREN)
            val expr = expression
            eat(RPAREN)
            return new Strof(expr)
          }
          val lhs = expression
          if (currentToken.kind == LBRACKET) {
            readToken
            expression
            eat(RBRACKET)
          } else if (currentToken.kind == DOT) {
            readToken

            currentToken.kind match {
              case LENGTH => {
                return new ArrayLength(lhs)
              }
              case IDKIND => {
                readToken
                eat(LPAREN)
                expression
                while (currentToken.kind == COMMA) {
                  expression
                }
                readToken
                eat(RPAREN)
              }
              case AND => {
                readToken
                return new And(lhs, expression)
              }
              case OR => {
                readToken
                return new Or(lhs, expression)
              }
              case EQUALS => {
                readToken
                return new Equals(lhs, expression)
              }
              case LESSTHAN => {
                readToken
                return new LessThan(lhs, expression)
              }
              case PLUS => {
                readToken
                return new Plus(lhs, expression)
              }
              case MINUS => {
                readToken
                return new Minus(lhs, expression)
              }
              case TIMES => {
                readToken
                return new Times(lhs, expression)
              }
              case DIV => {
                readToken
                return new Div(lhs, expression)
              }
            }

            expression
          }
        }
        fatal("Didn't catch " + currentToken)
      }

      def identifier = {
        println("ident " + currentToken.asInstanceOf[ID].value)
        val ident = new Identifier(currentToken.asInstanceOf[ID].value)
        readToken
        ident
      }

      mainMethod match {
        case Some(m) => new Program(m, classDeclList.toList)
        case None => fatal("No main method")
      }
    }

    readToken
    val tree = parseGoal
    terminateIfErrors
    tree
  }
}
