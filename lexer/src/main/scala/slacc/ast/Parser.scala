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
        eat(CLASS)
        val ident = identifier
        var parent: Option[Identifier] = None
        if (currentToken.kind == LESSTHAN) {
          eat(LESSTHAN)
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
          argsList += new Formal(argType, argIdent)
          while (currentToken.kind == COMMA) {
            eat(COMMA)
            argIdent = identifier
            eat(COLON)
            argType = typeTree
            argsList += new Formal(argType, argIdent)
          }
        }
        eat(RPAREN)
        eat(COLON)
        val retType = typeTree
        eat(EQSIGN)
        eat(LBRACE)
        while (currentToken.kind == VAR) {
          varList += varDeclaration
        }
        exprList += expression
        while (currentToken.kind == SEMICOLON) {
          eat(SEMICOLON)
          exprList += expression
        }
        eat(RBRACE)

        new MethodDecl(retType, ident, argsList.toList, varList.toList,
          exprList.toList, exprList.toList.last)
      }

      def typeTree = {
        if (currentToken.kind == INT) {
          eat(INT)
          if (currentToken.kind == LBRACKET) {
            eat(LBRACKET)
            eat(RBRACKET)
            new IntArrayType
          } else {
            new IntType
          }
        } else if (currentToken.kind == BOOLEAN) {
          eat(BOOLEAN)
          new BooleanType
        } else if (currentToken.kind == STRING) {
          eat(STRING)
          new StringType
        } else if (currentToken.kind == UNIT) {
          eat(UNIT)
          new UnitType
        } else {
          identifier
        }
      }

      def expression: ExprTree = {
        val expr = expr_helper
        currentToken.kind match {
          case DOT => {
            eat(DOT)
            currentToken.kind match {
              case LENGTH => {
                eat(LENGTH)
                return new ArrayLength(expr)
              }
              case IDKIND => {
                val ident = new Identifier(currentToken.asInstanceOf[ID].value)
                eat(IDKIND)
                eat(LPAREN)
                val args = new ListBuffer[ExprTree]()
                if (currentToken.kind != RPAREN) {
                  var expr_arg = expression
                  args += expr_arg
                  while (currentToken.kind == COMMA) {
                    eat(COMMA)
                    expr_arg = expression
                    args += expr_arg
                  }
                }
                eat(RPAREN)
                return new MethodCall(expr, ident, args.toList)
              }
            }
          }
          case LBRACKET => {
            eat(LBRACKET)
            val idx = expression
            eat(RBRACKET)
            return new ArrayRead(expr, idx)
          }
          case AND => {
            eat(AND)
            return new And(expr, expression)
          }
          case OR => {
            eat(OR)
            return new Or(expr, expression)
          }
          case EQUALS => {
            eat(EQUALS)
            return new Equals(expr, expression)
          }
          case LESSTHAN => {
            eat(LESSTHAN)
            return new LessThan(expr, expression)
          }
          case PLUS => {
            eat(PLUS)
            return new Plus(expr, expression)
          }
          case MINUS => {
            eat(MINUS)
            return new Minus(expr, expression)
          }
          case TIMES => {
            eat(TIMES)
            return new Times(expr, expression)
          }
          case DIV => {
            eat(DIV)
            return new Div(expr, expression)
          }
          case _ => {
            return expr
          }
        }
      }

      def expr_helper: ExprTree = {
        println("matching " + currentToken.kind)
        currentToken.kind match {
          case INTLITKIND => {
            val value = currentToken.asInstanceOf[INTLIT].value
            eat(INTLITKIND)
            return new IntLit(value)
          }
          case STRLITKIND => {
            val value = currentToken.asInstanceOf[STRLIT].value
            eat(STRLITKIND)
            return new StringLit(value)
          }
          case TRUE => {
            eat(TRUE)
            return new True()
          }
          case FALSE => {
            eat(FALSE)
            return new False()
          }
          case IDKIND => {
            val ident = identifier
            if (currentToken.kind == EQSIGN) {
              eat(EQSIGN)
              return new Assign(ident, expression)
            } else if (currentToken.kind == LBRACKET) {
              eat(LBRACKET)
              val index = expression
              eat(RBRACKET)
              eat(EQSIGN)
              return new ArrayAssign(ident, index, expression)
            } else {
              return ident
            }
          }
          case SELF => {
            eat(SELF)
            return new Self()
          }
          case NEW => {
            eat(NEW)
            if (currentToken.kind == INT) {
              eat(INT)
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
            eat(BANG)
            return new Not(expression)
          }
          case LPAREN => {
            eat(LPAREN)
            expression
            eat(RPAREN)
          }
          case LBRACE => {
            eat(LBRACE)
            val block = new ListBuffer[ExprTree]()
            if (currentToken.kind != RBRACE) {
              block += expression
              while (currentToken.kind == SEMICOLON) {
                eat(SEMICOLON)
                block += expression
              }
            }
            eat(RBRACE)
          }
          case IF => {
            eat(IF)
            eat(LPAREN)
            val cond = expression
            eat(RPAREN)
            val thn = expression
            var els: Option[ExprTree] = None
            if (currentToken.kind == ELSE) {
              eat(ELSE)
              els = Some(expression)
            }
            return new If(cond, thn, els)
          }
          case WHILE => {
            eat(WHILE)
            eat(LPAREN)
            val cond = expression
            eat(RPAREN)
            val body = expression
            return new While(cond, body)
          }
          case PRINTLN => {
            eat(PRINTLN)
            eat(LPAREN)
            val expr = expression
            eat(RPAREN)
            return new Println(expr)
          }
          case STROF => {
            eat(STROF)
            eat(LPAREN)
            val expr = expression
            eat(RPAREN)
            return new Strof(expr)
          }
          case _ => {
            println(":(((")
          }
          /*val lhs = expression
          if (currentToken.kind == LBRACKET) {
            eat(LBRACKET)
            expression
            eat(RBRACKET)
          } else if (currentToken.kind == DOT) {
            eat(DOT)
            currentToken.kind match {
              case LENGTH => {
                return new ArrayLength(lhs)
              }
              case IDKIND => {
                eat(IDKIND)
                eat(LPAREN)
                if (currentToken.kind != RPAREN) {
                  expression
                  while (currentToken.kind == COMMA) {
                    eat(COMMA)
                    expression
                  }
                }
                eat(RPAREN)
              }
            }
          } else {
            currentToken.kind match {
              case AND => {
                eat(AND)
                return new And(lhs, expression)
              }
              case OR => {
                eat(OR)
                return new Or(lhs, expression)
              }
              case EQUALS => {
                eat(EQUALS)
                return new Equals(lhs, expression)
              }
              case LESSTHAN => {
                eat(LESSTHAN)
                return new LessThan(lhs, expression)
              }
              case PLUS => {
                eat(PLUS)
                return new Plus(lhs, expression)
              }
              case MINUS => {
                eat(MINUS)
                return new Minus(lhs, expression)
              }
              case TIMES => {
                eat(TIMES)
                return new Times(lhs, expression)
              }
              case DIV => {
                eat(DIV)
                return new Div(lhs, expression)
              }
            }
          }*/
        }
        fatal("Didn't catch " + currentToken)
      }

      def identifier = {
        println("ident " + currentToken.asInstanceOf[ID].value)
        val ident = new Identifier(currentToken.asInstanceOf[ID].value)
        eat(IDKIND)
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
