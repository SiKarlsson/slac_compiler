package slacc
package code

import ast.Trees._
import analyzer.Symbols._
import analyzer.Types._
import analyzer.NameAnalysis._
import cafebabe._
import AbstractByteCodes.{New => _, _}
import ByteCodes._
import utils._

object CodeGeneration extends Pipeline[Program, Unit] {

  def run(ctx: Context)(prog: Program): Unit = {
    import ctx.reporter._

    /** Writes the proper .class file in a given directory. An empty string for dir is equivalent to "./". */
    def generateClassFile(sourceName: String, ct: ClassDecl, dir: String): Unit = {
      val classFile = new ClassFile(ct.id.value, None)
      classFile.setSourceFile(sourceName)
      classFile.addDefaultConstructor
      for (vari <- ct.vars) {
          classFile.addField(typeString(vari.tpe), vari.id.value)
      }
      ct.methods foreach {
        meth => {
          if (ct.id.value == "Main") {
            val mainHandler = classFile.addMainMethod.codeHandler
            generateMethodCode(meth)(mainHandler)
          } else {
            val mh: MethodHandler = classFile.addMethod(typeString(meth.retType), meth.id.value, parameterString(meth.args))
            generateMethodCode(meth)(mh.codeHandler)
          }
        }
      }
      classFile.writeToFile(dir + "/" + ct.id.value + ".class")
    }

    // a mapping from variable symbols to positions in the local variables
    // of the stack frame
    def generateMethodCode(mt: MethodDecl)(implicit ch: CodeHandler): Unit = {
      var variables = Map[Symbol, Int]()
      def addVariable(sym: Symbol)(implicit ch: CodeHandler) {
        variables += (sym -> ch.getFreshVar)
      }

      val methSym = mt.getSymbol

      mt.args foreach { mArgs => addVariable(mArgs.id.getSymbol) }

      mt.getSymbol.members foreach { mVars => addVariable(mVars._2) }

      for (e <- mt.exprs :+ mt.retExpr) {
        generateExprCode(e)(ch, variables)
        e.getType match {
          case TUnit => { }
          case _ => { ch << POP }
        }

      }

      ch << (getTypeOfTypeTree(mt.retType, ctx.reporter) match {
        case TInt => { IRETURN }
        case TUnit => { RETURN }
        case _ => { ARETURN }
      })

      ch.print
      ch.freeze
    }

    def generateExprCode(e: ExprTree)(implicit ch: CodeHandler, variables: Map[Symbol, Int]): Unit = {
      e match {
        case And(lhs, rhs) => {
          generateExprCode(lhs)
          generateExprCode(rhs)
          ch << IAND
        }
        case Or(lhs, rhs) => {
          generateExprCode(lhs)
          generateExprCode(rhs)
          ch << IOR
        }
        case Plus(lhs, rhs) => {
          e.getType match {
            case TString => {
              // TODO: Concatenating strings
              sys.error("String concatenation is not implemented yet")
            }
            case TInt => {
              generateExprCode(lhs)
              generateExprCode(rhs)
              ch << IADD
            }
            case _ => sys.error("Plus is not supported for " + e)
          }
        }
        case Minus(lhs, rhs) => {
          generateExprCode(lhs)
          generateExprCode(rhs)
          ch << ISUB
        }
        case Times(lhs, rhs) => {
          generateExprCode(lhs)
          generateExprCode(rhs)
          ch << IMUL
        }
        case Div(lhs, rhs) => {
          generateExprCode(lhs)
          generateExprCode(rhs)
          ch << IDIV
        }
        case LessThan(lhs, rhs) => {
          val label1 = ch.getFreshLabel("lessthan-return-1")
          val label2 = ch.getFreshLabel("lessthan-after")
          generateExprCode(rhs)
          generateExprCode(lhs)
          ch << IfLe(label1) << Ldc(0) << Goto(label2) <<
            Label(label1) << Ldc(1) << Label(label2)
        }
        case Equals(lhs, rhs) => {
          generateExprCode(lhs)
          generateExprCode(rhs)
        }
        case ArrayRead(arr, index) => {

        }
        case ArrayLength(arr) => {
          generateExprCode(arr)
          ch << ARRAYLENGTH
        }
        case MethodCall(obj, meth, args) => {

        }
        case IntLit(value) => {
          ch << Ldc(value)
        }
        case StringLit(value) => {
          ch << Ldc(value)
        }
        case True() => {
          ch << ICONST_1
        }
        case False() => {
          ch << ICONST_0
        }
        case Identifier(value) => {
          e.asInstanceOf[Identifier].getType match {
            case TInt => { ch << ILoad(variables(e.asInstanceOf[Identifier].getSymbol)) }
            case _ => { ch << ALoad(variables(e.asInstanceOf[Identifier].getSymbol)) }
          }
        }
        case Self() => {

        }
        case NewIntArray(size) => {
          println(size)
          generateExprCode(size)
          ch << NewArray("I")
        }
        case New(tpe) => {

        }
        case Not(tpe) => {
          generateExprCode(tpe)
          val label1 = ch.getFreshLabel("not-return-1")
          val label2 = ch.getFreshLabel("not-return-0")
          ch << Ldc(1) << If_ICmpNe(label1) << Ldc(0) << Goto(label2) <<
            Label(label1) << Ldc(1) << Label(label2)
        }
        case Block(exprs) => {
          exprs foreach {
            b => { generateExprCode(b) }
          }
        }
        case If(expr, thn, els) => {
          generateExprCode(expr)
          val label1 = ch.getFreshLabel("if-then")
          val label2 = ch.getFreshLabel("if-else")
          ch << Ldc(1) << If_ICmpEq(label1)
          els match {
            case Some(e) => {
              generateExprCode(e)
            }
            case _ => {}
          }
          ch << Goto(label2) << Label(label1)
          generateExprCode(thn)
          ch << Label(label2)
        }
        case While(cond, body) => {
          val label1 = ch.getFreshLabel("while-cond")
          val label2 = ch.getFreshLabel("while-body")
          val label3 = ch.getFreshLabel("while-after")
          ch << Label(label1)
          generateExprCode(cond)
          ch << Ldc(1) << If_ICmpEq(label2) << Goto(label3) << Label(label2) << POP
          generateExprCode(body)
          ch << Goto(label1) << Label(label3) << POP
        }
        case Println(value) => {
          ch << GetStatic("java/lang/System", "out", "Ljava/io/PrintStream;") <<
            Ldc(value.asInstanceOf[StringLit].value.toString) <<
            InvokeVirtual("java/io/PrintStream", "println", "(Ljava/lang/String;)V")
        }
        case Assign(id, expr) => {
          // The value to be stored will be on the top of the stack
          generateExprCode(expr)
          id.asInstanceOf[Identifier].getType match {
            case TInt => {
              ch << { IStore(variables(id.asInstanceOf[Identifier].getSymbol)) }
            }
            case TIntArray => {
              ch << { AStore(variables(id.asInstanceOf[Identifier].getSymbol)) }
            }
            case _ => {
              ch << { AStore(variables(id.asInstanceOf[Identifier].getSymbol)) }
            }
          }
        }
        case ArrayAssign(id, index, expr) => {
          ch << ILoad(variables(id.asInstanceOf[Identifier].getSymbol))
          generateExprCode(index)
          generateExprCode(expr)
          ch << IASTORE
        }
        case Strof(expr) => {

        }
      }
    }

    val outDir = ctx.outDir.map(_.getPath+"/").getOrElse("./")

    val f = new java.io.File(outDir)
    if (!f.exists()) {
      f.mkdir()
    }

    val sourceName = ctx.files.head.getName

    // output code
    prog.classes foreach {
      ct => generateClassFile(sourceName, ct, outDir)
    }

    generateClassFile(sourceName, mainClassDecl, outDir)

  }

  def typeString(retType: TypeTree): String = {
    retType match {
      case IntType() => {
        "I"
      }
      case StringType() => {
        "Ljava/lang/String"
      }
      case UnitType() => {
        "V"
      }
      case BooleanType() => {
        "Z"
      }
      case IntArrayType() => {
        "[I"
      }
      case Identifier(value) => {
        "L".concat(value)
      }
      case _ => {
        sys.error(retType + " has no type!")
      }
    }
  }

  def parameterString(args: List[Formal]): String = {
    var paramStr = "";
    for (arg <- args) {
      paramStr = paramStr.concat(typeString(arg.tpe))
    }
    paramStr
  }
}
