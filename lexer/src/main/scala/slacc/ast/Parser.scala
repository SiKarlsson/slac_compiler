package slacc
package ast

import utils._
import Trees._
import lexer._
import lexer.Tokens._

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

      val classDeclList = List()
      var mainMethod: Option[MainMethod] = None

      while (currentToken.kind == CLASS) {
        classDeclList :+ classDeclaration
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
        val vars = List()
        while (currentToken.kind == VAR) {
          vars :+ varDeclaration
        }
        val methods = List()
        while (currentToken.kind == METHOD) {
          methods :+ methodDeclaration
        }
        new ClassDecl(ident, parent, vars, methods)
      }

      def varDeclaration = {
        // var Identifier : Type ;
        if (currentToken.kind == VAR) {
          readToken
          val ident = identifier
          eat(COLON)
          val tt = typeTree
          eat(SEMICOLON)
          new VarDecl(tt, ident)
        }
      }

      def methodDeclaration = {
        /* method Identifier ( ( Identifier : Type ( , Identifier : Type )* )? )
         : Type = { ( VarDeclaration )* Expression ( ; Expression )* } */
        eat(METHOD)
        val ident = identifier
        val argsList = List()
        val varList = List()
        val exprList = List()
        eat(LPAREN)
        if (currentToken.kind == IDKIND) {
          var argIdent = identifier
          eat(COLON)
          var argType = typeTree
          argsList :+ new Formal(argType, argIdent)
          while (currentToken.kind == COMMA) {
            argIdent = identifier
            eat(COLON)
            argType = typeTree
            argsList :+ new Formal(argType, argIdent)
          }
        }
        eat(RPAREN)
        eat(COLON)
        val retType = typeTree
        eat(EQUALS)
        eat(LBRACE)
        while (currentToken.kind == VAR) {
          varList :+ varDeclaration
        }
        exprList :+ expression
        while (currentToken.kind == SEMICOLON) {
          exprList :+ expression
        }
        eat(RBRACE)

        val meth = new MethodDecl(retType, ident, argsList, varList,
          exprList.dropRight(1), exprList.last)
        if (1 == 1) {
          mainMethod = Some(new MainMethod(meth))
        }
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
          new BooleanType
        } else if (currentToken.kind == STRLITKIND) {
          new StringType
        } else if (currentToken.kind == UNIT) {
          new UnitType
        }
        identifier
      }

      def expression: ExprTree = {
        if (currentToken.kind == TRUE) {
          new True()
        } else if (currentToken.kind == INTLITKIND) {
          new IntLit(currentToken.asInstanceOf[INTLIT].value)
        } else if (currentToken.kind == STRLITKIND) {
          new StringLit(currentToken.asInstanceOf[STRLIT].value)
        } else if (currentToken.kind == FALSE) {
          new False()
        } else if (currentToken.kind == SELF) {
          new Self()
        } else if (currentToken.kind == IDKIND) {
          identifier
        } else if (currentToken.kind == NEW) {
          readToken
          if (currentToken.kind == INT) {
            readToken
            eat(LBRACKET)
            val expr = expression
            eat(RBRACKET)
            new NewIntArray(expr)
          } else {
            val ident = identifier
            eat(RBRACKET)
            eat(LBRACKET)
            new New(ident)
          }
        } else if (currentToken.kind == BANG) {
          readToken
          new Not(expression)
        } else if (currentToken.kind == LPAREN) {
          eat(LPAREN)
          expression
          eat(RPAREN)
        } else if (currentToken.kind == LBRACE) {
          readToken
          val block = List()
          block :+ expression
          while (currentToken.kind == SEMICOLON) {
            block :+ expression
          }
          eat(LBRACE)
        } else if (currentToken.kind == IF) {
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
        } else if (currentToken.kind == WHILE) {
          readToken
          eat(LPAREN)
          val cond = expression
          eat(RPAREN)
          val body = expression
          new While(cond, body)
        } else if (currentToken.kind == PRINTLN) {
          readToken
          eat(LPAREN)
          val expr = expression
          eat(RPAREN)
          new Println(expr)
        } else if (currentToken.kind == STROF) {
          readToken
          eat(LPAREN)
          val expr = expression
          eat(RPAREN)
          new Strof(expr)
        } else {
          val lhs = expression
          if (currentToken.kind == LBRACKET) {
            readToken
            expression
            eat(RBRACKET)
          } else if (currentToken.kind == DOT) {
            readToken

            currentToken.kind match {
              case LENGTH => {
                new ArrayLength(lhs)
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
                new And(lhs, expression)
              }
              case OR => {
                new Or(lhs, expression)
              }
              case EQUALS => {
                new Equals(lhs, expression)
              }
              case LESSTHAN => {
                new LessThan(lhs, expression)
              }
              case PLUS => {
                new Plus(lhs, expression)
              }
              case MINUS => {
                new Minus(lhs, expression)
              }
              case TIMES => {
                new Times(lhs, expression)
              }
              case DIV => {
                new Div(lhs, expression)
              }
            }

            expression
          }
        }
        ???
      }

      def identifier = {
        val ident = new Identifier(currentToken.asInstanceOf[ID].value)
        readToken
        ident
      }

      mainMethod match {
        case Some(m) => new Program(m, classDeclList)
        case None => fatal("No main method")
      }
    }

    readToken
    val tree = parseGoal
    terminateIfErrors
    tree
  }
}
