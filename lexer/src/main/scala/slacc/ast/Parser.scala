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

      def classDeclaration = {
        // class Identifier ( <: Identifier )? { ( VarDeclaration )* ( MethodDeclaration )* }
        if (currentToken == CLASS) {
          readToken
          val ident = identifier
          var parent: Option[Identifier] = None
          if (currentToken == LESSTHAN) {
            eat(COLON)
            //eat(Token(COLON))
            parent = identifier
          }
          eat(LBRACE)
          val vars = List();
          while (currentToken == VAR) {
            vars :+ varDeclaration
          }
          val methods = List();
          while (currentToken == METHOD) {
            methods :+ methodDeclaration
          }
          new ClassDecl(ident, parent, vars, methods)
        }
      }

      def varDeclaration = {
        // var Identifier : Type ;
        if (currentToken == VAR) {
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
        if (currentToken == IDKIND) {
          var argIdent = identifier
          eat(COLON)
          var argType = typeTree
          argsList :+ new Formal(argType, argIdent)
          while (currentToken == COMMA) {
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
        while (currentToken == VAR) {
          varList :+ varDeclaration
        }
        exprList :+ expression
        while (currentToken == SEMICOLON) {
          exprList :+ expression
        }
        eat(RBRACE)

        new MethodDecl(retType, ident, argsList, varList, exprList.dropRight(1), exprList.last)
      }

      def typeTree = {
        if (currentToken == INTLITKIND) {
          readToken
          if (currentToken == LBRACKET) {
            eat(RBRACKET)
            new IntArrayType
          } else {
            new IntType
          }
        } else if (currentToken == BOOLEAN) {
          new BooleanType
        } else if (currentToken == STRLITKIND) {
          new StringType
        } else if (currentToken == UNIT) {
          new UnitType
        }
        identifier
      }

      def expression: ExprTree = {
        if (currentToken == TRUE) {
          new True()
        } else if (currentToken == INTLITKIND) {
          new IntLit(currentToken.asInstanceOf[INTLIT].value)
        } else if (currentToken == STRLITKIND) {
          new StringLit(currentToken.asInstanceOf[STRLIT].value)
        } else if (currentToken == FALSE) {
          new False()
        } else if (currentToken == SELF) {
          new Self()
        } else if (currentToken == IDKIND) {
          identifier
        } else if (currentToken == NEW) {
          readToken
          if (currentToken == INT) {
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
        } else if (currentToken == BANG) {
          readToken
          new Not(expression)
        } else if (currentToken == LPAREN) {
          eat(LPAREN)
          expression
          eat(RPAREN)
        } else if (currentToken == LBRACE) {
          readToken
          val block = List()
          block :+ expression
          while (currentToken == SEMICOLON) {
            block :+ expression
          }
          eat(LBRACE)
        } else if (currentToken == IF) {
          readToken
          eat(LPAREN)
          val cond = expression
          eat(RPAREN)
          val thn = expression
          var els: Option[ExprTree] = None
          if (currentToken == ELSE) {
            readToken
            els = Some(expression)
          }
          new If(cond, thn, els)
        } else if (currentToken == WHILE) {
          readToken
          eat(LPAREN)
          val cond = expression
          eat(RPAREN)
          val body = expression
          new While(cond, body)
        } else if (currentToken == PRINTLN) {
          readToken
          eat(LPAREN)
          val expr = expression
          eat(RPAREN)
          new Println(expr)
        } else if (currentToken == STROF) {
          readToken
          eat(LPAREN)
          val expr = expression
          eat(RPAREN)
          new Strof(expr)
        } else {
          val lhs = expression
          if (currentToken == LBRACKET) {
            expression
            eat(RBRACKET)
          } else if (currentToken == DOT) {
            readToken
            if (currentToken == LENGTH) {
              new ArrayLength(lhs);
            } else if (currentToken == IDKIND) {
              eat(LPAREN)
              expression
              while (currentToken == COMMA) {
                expression
              }
              eat(RPAREN)
            } else if (currentToken == AND) {
              new And(lhs, expression)
            } else if (currentToken == OR) {
              new Or(lhs, expression)
            } else if (currentToken == EQUALS) {
              new Equals(lhs, expression)
            } else if (currentToken == LESSTHAN) {
              new LessThan(lhs, expression)
            } else if (currentToken == PLUS) {
              new Plus(lhs, expression)
            } else if (currentToken == MINUS) {
              new Minus(lhs, expression)
            } else if (currentToken == TIMES) {
              new Times(lhs, expression)
            } else if (currentToken == DIV) {
              new Div(lhs, expression)
            }
            expression
          }
        }

        ???
      }

      def identifier = {
        ???
      }

      ???

    }

    readToken
    val tree = parseGoal
    terminateIfErrors
    tree
  }
}
