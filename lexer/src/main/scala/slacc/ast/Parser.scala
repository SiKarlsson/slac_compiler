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
        println("METHOD LPAREN args RPAREN")
        eat(RPAREN)
        eat(COLON)
        val retType = typeTree
        eat(EQSIGN)
        eat(LBRACE)
        while (currentToken.kind == VAR) {
          varList += varDeclaration
        }

        do {
          if (currentToken.kind == SEMICOLON) {
            eat(SEMICOLON)
          }
          var expr = expression(None)
          while (currentToken.kind == DOT) {
            expr = expression(Some(expr))
          }
          exprList += expr
        } while (currentToken.kind == SEMICOLON)
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

      def expression(expr: Option[ExprTree]): ExprTree = {
        expr match {
          /* we have an expression, but the expression goes on */
          case Some(e) => {
            currentToken.kind match {
              /* expr. */
              case DOT => {
                eat(DOT)
                currentToken.kind match {
                  case LENGTH => {
                    eat(LENGTH)
                    return new ArrayLength(e)
                  }
                  case IDKIND => {
                    val ident = new Identifier(currentToken.asInstanceOf[ID].value)
                    eat(IDKIND)
                    eat(LPAREN)
                    val args = new ListBuffer[ExprTree]()
                    /* if currentToken is RPAREN, empty call */
                    if (currentToken.kind != RPAREN) {
                      /* otherwise, there is at least one argument */
                      do {
                        if (currentToken.kind == COMMA) {
                          /* separator of arguments */
                          eat(COMMA)
                        }
                        /* find a new expression */
                        var expr_arg = expression(None)
                        /* while no RPAREN or COMMA is encountered, the
                          expression goes on */
                        while (currentToken.kind != RPAREN && currentToken.kind != COMMA) {
                          expr_arg = expression(Some(expr_arg))
                        }
                        /* we've reached RPAREN or COMMA, new argument or end of
                          call */
                        args += expr_arg
                      } while (currentToken.kind == COMMA)
                    }
                    println(".identifier LPAREN args RPAREN")
                    eat(RPAREN)
                    return new MethodCall(e, ident, args.toList)
                  }
                }
              }
              case LBRACKET => {
                eat(LBRACKET)
                val idx = expression(None)
                eat(RBRACKET)
                return new ArrayRead(e, idx)
              }
              case AND => {
                eat(AND)
                return new And(e, expression(None))
              }
              case OR => {
                eat(OR)
                return new Or(e, expression(None))
              }
              case EQUALS => {
                eat(EQUALS)
                return new Equals(e, expression(None))
              }
              case LESSTHAN => {
                eat(LESSTHAN)
                return new LessThan(e, expression(None))
              }
              case PLUS => {
                eat(PLUS)
                return new Plus(e, expression(None))
              }
              case MINUS => {
                eat(MINUS)
                return new Minus(e, expression(None))
              }
              case TIMES => {
                eat(TIMES)
                return new Times(e, expression(None))
              }
              case DIV => {
                eat(DIV)
                return new Div(e, expression(None))
              }
              case _ => {
                return e
              }
            }
          }
          /* new expression */
          case None => {
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
                  return new Assign(ident, expression(None))
                } else if (currentToken.kind == LBRACKET) {
                  eat(LBRACKET)
                  val index = expression(None)
                  eat(RBRACKET)
                  currentToken.kind match {
                    case EQSIGN => {
                      eat(EQSIGN)
                      return new ArrayAssign(ident, index, expression(None))
                    }
                    case _ => {
                      return new ArrayRead(ident, index)
                    }
                  }
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
                  val expr = expression(None)
                  eat(RBRACKET)
                  return new NewIntArray(expr)
                } else {
                  val ident = identifier
                  eat(LPAREN)
                  println("NEW LPAREN RPAREN")
                  eat(RPAREN)
                  return new New(ident)
                }
              }
              case BANG => {
                eat(BANG)
                return new Not(expression(None))
              }
              case LPAREN => {
                eat(LPAREN)
                var expr = expression(None)
                while (currentToken.kind != RPAREN) {
                  expr = expression(Some(expr))
                }
                println("LPAREN expression RPAREN")
                eat(RPAREN)
                return expr
              }
              case LBRACE => {
                eat(LBRACE)
                val block = new ListBuffer[ExprTree]()
                if (currentToken.kind != RBRACE) {
                  block += expression(None)
                  while (currentToken.kind == SEMICOLON) {
                    eat(SEMICOLON)
                    block += expression(None)
                  }
                }
                eat(RBRACE)
              }
              case IF => {
                eat(IF)
                eat(LPAREN)
                var cond = expression(None)
                while (currentToken.kind != RPAREN) {
                  cond = expression(Some(cond))
                }
                println("IF LPAREN expression RPAREN")
                eat(RPAREN)
                val thn = expression(None)
                var els: Option[ExprTree] = None
                if (currentToken.kind == ELSE) {
                  eat(ELSE)
                  els = Some(expression(None))
                }
                return new If(cond, thn, els)
              }
              case WHILE => {
                eat(WHILE)
                eat(LPAREN)
                var cond = expression(None)
                while (currentToken.kind != RPAREN) {
                  cond = expression(Some(cond))
                }
                println("WHILE LPAREN expression RPAREN")
                eat(RPAREN)
                val body = expression(None)
                return new While(cond, body)
              }
              case PRINTLN => {
                eat(PRINTLN)
                eat(LPAREN)
                var expr = expression(None)
                while (currentToken.kind != RPAREN) {
                  expr = expression(Some(expr))
                }
                println("PRINTLN LPAREN expression RPAREN")
                eat(RPAREN)
                return new Println(expr)
              }
              case STROF => {
                eat(STROF)
                eat(LPAREN)
                var expr = expression(None)
                while (currentToken.kind != RPAREN) {
                  expr = expression(Some(expr))
                }
                println("STROF LPAREN expression RPAREN")
                eat(RPAREN)
                return new Strof(expr)
              }
              case _ => {
                println(":(((")
              }
            }
          }
          fatal("Didn't catch " + currentToken)
        }
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
