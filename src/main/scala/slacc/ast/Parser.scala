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
        do {
          if (currentToken.kind == SEMICOLON) {
            eat(SEMICOLON)
          }
          var expr = parseExpr1
          exprList += expr
        } while (currentToken.kind == SEMICOLON)
        eat(RBRACE)

        new MethodDecl(retType, ident, argsList.toList, varList.toList,
          exprList.toList.dropRight(1), exprList.toList.last)
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

      def parseExpr1: ExprTree = {
        var e1 = parseExpr2
        while (currentToken.kind == OR) {
          eat(OR)
          var e2 = parseExpr2
          e1 = new Or(e1, e2)
        }
        currentToken.kind match {
          case DOT => {
            eat(DOT)
            currentToken.kind match {
              case LENGTH => {
                eat(LENGTH)
                e1 = new ArrayLength(e1)
              }
              case IDKIND => {
                var ident = identifier
                val exprList = new ListBuffer[ExprTree]()
                eat(LPAREN)
                do {
                  if (currentToken.kind == COMMA) eat(COMMA)
                  val e2 = parseExpr1
                  exprList += e2
                } while (currentToken.kind == COMMA)
                eat(RPAREN)

                e1 = new MethodCall(e1, ident, exprList.toList)
              }
              case _ => fatal("fatal")
            }
          }
          case LBRACKET => {
            eat(LBRACKET)
            val e2 = parseExpr1
            eat(RBRACKET)
            e1 = new ArrayRead(e1, e2)
          }
          case _ => { }
        }
        e1
      }

      def parseExpr2: ExprTree = {
        var e1 = parseExpr3
        while (currentToken.kind == AND) {
          eat(AND)
          var e2 = parseExpr3
          e1 = new And(e1, e2)
        }
        e1
      }

      def parseExpr3: ExprTree = {
        var e1 = parseExpr4
        while (currentToken.kind == EQUALS || currentToken.kind == LESSTHAN) {
          var equals = (currentToken.kind == EQUALS)
          readToken
          var e2 = parseExpr4
          if (equals) {
              e1 = new Equals(e1, e2)
          } else {
            e1 = new LessThan(e1, e2)
          }
        }
        e1
      }

      def parseExpr4: ExprTree = {
        var e1 = parseExpr5
        while (currentToken.kind == PLUS || currentToken.kind == MINUS) {
          var plus = (currentToken.kind == PLUS)
          readToken
          var e2 = parseExpr5
          if (plus) {
            e1 = new Plus(e1, e2)
          } else {
            e1 = new Minus(e1, e2)
          }
        }
        e1
      }

      def parseExpr5: ExprTree = {
        var e1 = expression
        while (currentToken.kind == TIMES || currentToken.kind == DIV) {
          var times = (currentToken.kind == TIMES)
          readToken
          var e2 = expression
          if (times) {
            e1 = new Times(e1, e2)
          } else {
            e1 = new Div(e1, e2)
          }
        }
        parseExpr6(e1)
      }

      def parseExpr6(expr: ExprTree): ExprTree = {
        var e1 = expr
        while (currentToken.kind == DOT || currentToken.kind == LBRACKET) {
          currentToken.kind match {
            case DOT => {
              eat(DOT)
              currentToken.kind match {
                case LENGTH => {
                  eat(LENGTH)
                  e1 = new ArrayLength(e1)
                }
                case IDKIND => {
                  var ident = identifier
                  val exprList = new ListBuffer[ExprTree]()
                  eat(LPAREN)
                  if (currentToken.kind != RPAREN) {
                    do {
                      if (currentToken.kind == COMMA) eat(COMMA)
                      val e2 = parseExpr1
                      exprList += e2
                    } while (currentToken.kind == COMMA)
                  }
                  eat(RPAREN)

                  e1 = new MethodCall(e1, ident, exprList.toList)
                }
                case _ => fatal("fatal")
              }
            }
            case LBRACKET => {
              eat(LBRACKET)
              val e2 = parseExpr1
              eat(RBRACKET)
              e1 = new ArrayRead(e1, e2)
            }
            case _ => { e1 }
          }
        }
        e1
      }

      def expression: ExprTree = {
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
            new True()
          }
          case FALSE => {
            eat(FALSE)
            new False()
          }
          case IDKIND => {
            val id = identifier
            if (currentToken.kind == EQSIGN) {
              eat(EQSIGN)
              val e1 = parseExpr1
              new Assign(id, e1)
            } else if (currentToken.kind == LBRACKET){
              eat(LBRACKET)
              val e1 = parseExpr1
              eat(RBRACKET)
              if (currentToken.kind == EQSIGN) {
                eat(EQSIGN)
                val e2 = parseExpr1
                new ArrayAssign(id, e1, e2)
              } else {
                new ArrayRead(id, e1)
              }
            } else {
              id
            }
          }
          case SELF => {
            eat(SELF)
            new Self()
          }
          case NEW => {
            eat(NEW)
            if (currentToken.kind == INT) {
              eat(INT)
              eat(LBRACKET)
              val expr = parseExpr1
              eat(RBRACKET)
              new NewIntArray(expr)
            } else {
              val ident = identifier
              eat(LPAREN)
              eat(RPAREN)
              new New(ident)
            }
          }
          case BANG => {
            eat(BANG)
            new Not(parseExpr1)
          }
          case LPAREN => {
            eat(LPAREN)
            var expr = parseExpr1
            eat(RPAREN)
            expr
          }
          case LBRACE => {
            eat(LBRACE)
            val block = new ListBuffer[ExprTree]()
            do {
              if (currentToken.kind == SEMICOLON) {
                eat(SEMICOLON)
              }
              block += parseExpr1
            } while (currentToken.kind == SEMICOLON)
            eat(RBRACE)
            Block(block.toList)
          }
          case IF => {
            eat(IF)
            eat(LPAREN)
            var cond = parseExpr1
            eat(RPAREN)
            val thn = parseExpr1
            var els: Option[ExprTree] = None
            if (currentToken.kind == ELSE) {
              eat(ELSE)
              els = Some(parseExpr1)
            }
            new If(cond, thn, els)
          }
          case WHILE => {
            eat(WHILE)
            eat(LPAREN)
            var cond = parseExpr1
            eat(RPAREN)
            val body = parseExpr1
            new While(cond, body)
          }
          case PRINTLN => {
            eat(PRINTLN)
            eat(LPAREN)
            var expr = parseExpr1
            eat(RPAREN)
            new Println(expr)
          }
          case STROF => {
            eat(STROF)
            eat(LPAREN)
            var expr = parseExpr1
            eat(RPAREN)
            new Strof(expr)
          }
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
