package slacc
package lexer

import utils._
import scala.io.Source
import java.io.File
import scala.collection.mutable.Stack

object Lexer extends Pipeline[File, Iterator[Token]] {
  import Tokens._

  def run(ctx: Context)(f: File): Iterator[Token] = {
    val source = Source.fromFile(f)
    import ctx.reporter._

    new Iterator[Token] {

      var matching = true
      val consumed = Stack[Char]()

      def hasNext = {
        source.hasNext || consumed.size > 0
      }

      def next: Token = {
        var currMatch = ""
        var longestMatch = new Token(BAD)

        while (hasNext && matching) {
          var currChar: Char = readChar
          if (currChar == '\n' || currChar == ' ' || currChar == '\t') {
            return longestMatch
            if (hasNext) {
              next
            } else {
              new Token(EOF)
            }
          }
          currMatch += currChar
          val tok = matchToken(currMatch)
          if (tok.toString == "BAD" && longestMatch.toString != "BAD") {
            /* the new match is "BAD" and we have a longest match already,
              no more matches possible*/
            consumed.push(currChar)
            matching = false
          }
          if (tok.toString != "BAD") {
            /* found a match! update longest match */
            longestMatch = tok
          }
        }

        matching = true

        /* nothing more to read, EOF */
        if (!hasNext) {
          longestMatch = new Token(EOF)
        }

        longestMatch
      }

      def matchToken(s: String) = {
        s match {
          case ":" => new Token(COLON)
          case ";" => new Token(SEMICOLON)
          case "." => new Token(DOT)
          case "," => new Token(COMMA)
          case "=" => new Token(EQSIGN)
          case "==" => new Token(EQUALS)
          case "!" => new Token(BANG)
          case "(" => new Token(LPAREN)
          case ")" => new Token(RPAREN)
          case "[" => new Token(LBRACKET)
          case "]" => new Token(RBRACKET)
          case "{" => new Token(LBRACE)
          case "}" => new Token(RBRACE)
          case "&&" => new Token(AND)
          case "||" => new Token(OR)
          case "<" => new Token(LESSTHAN)
          case "+" => new Token(PLUS)
          case "-" => new Token(MINUS)
          case "*" => new Token(TIMES)
          case "/" => {
            new Token(DIV)
            if (hasNext) {
              var nextChar: Char = source.next
              if (nextChar == '/') {
                  /* // comment, look for line break */
                  while (nextChar != '\n') {
                    nextChar = source.next
                  }
                  next
              } else {
                new Token(DIV)
              }
            } else {
              new Token(BAD)
            }
          }
          case "class" => new Token(CLASS)
          case "method" => new Token(METHOD)
          case "var" => new Token(VAR)
          case "unit" => new Token(UNIT)
          case "string" => new Token(STRING)
          case "extends" => new Token(EXTENDS)
          case "int" => new Token(INT)
          case "boolean" => new Token(BOOLEAN)
          case "while" => new Token(WHILE)
          case "if" => new Token(IF)
          case "else" => new Token(ELSE)
          case "length" => new Token(LENGTH)
          case "true" => new Token(TRUE)
          case "false" => new Token(FALSE)
          case "self" => new Token(SELF)
          case "new" => new Token(NEW)
          case "println" => new Token(PRINTLN)
          case "strOf" => new Token(STROF)
          case "\"" => {
            var strLit: String = ""
            var sourceNext: String = source.next.toString
            while (hasNext && sourceNext != "\"") {
              strLit += sourceNext
              sourceNext = source.next.toString
            }
            new STRLIT(strLit)
          }
          case _ => new Token(BAD)
        }
      }

      def readChar = {
        if (consumed.size > 0) {
          consumed.pop
        } else {
          source.next
        }
      }
    }
  }
}

object DisplayTokens extends Pipeline[Iterator[Token], Iterator[Token]] {
  def run(ctx: Context)(tokens: Iterator[Token]): Iterator[Token] = {
    new Iterator[Token] {
      def hasNext = {
        tokens.hasNext
      }

      def next = {
        val n = tokens.next
        println(n+"("+n.line+":"+n.col+") ")
        n
      }
    }
  }
}
