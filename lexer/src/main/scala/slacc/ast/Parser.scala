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
        if (currentToken == Token(CLASS)) {
          readToken
          [identifier]
          if (currentToken == Token(LESSTHAN)) {
            eat(COLON)
            [identifier]
          } 
          eat(Token(LBRACE))
          while (currentToken == Token(VAR)) {
            [varDeclaration]
          }
          while (currentToken == Token(METHOD)) {
            [methodDeclaration]
          }
        }
      }

      def varDeclaration = {
        // var Identifier : Type ;
        if (currentToken == Token(VAR)) { 
          readToken
          identifier
          eat(Token(COLON))
          type
          eat(Token(SEMICOLON))
        }
      }

      def methodDeclaration = {

      }

      def type = {
        if (currentToken == Token(INT)) {
          readToken
          if (currentToken == Token(LBRACKET)) {
            eat(Token(RBRACKET))
            // Int array
          } else {
            // Int
          }
        } else if (currentToken == Token(BOOLEAN)) {
          // Boolean
        } else if (currentToken == Token(STR)) {
          // Str
        } else if (currentToken == Token(UNIT)) {
          // Unit
        } else {
          identifier
        }
      }

      def type expression = {

      }

      def type identifier = {

      }

    }

    readToken
    val tree = parseGoal
    terminateIfErrors
    tree
  }
}
