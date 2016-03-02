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
          var parent: Option[Identifier] = Some(???)
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
          new BoolType
        } else if (currentToken == STRLITKIND) {
          new StringType
        } else if (currentToken == UNIT) {
          new UnitType
        }
        identifier
      }

      def expression = {
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
