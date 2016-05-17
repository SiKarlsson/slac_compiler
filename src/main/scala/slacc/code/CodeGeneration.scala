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

      for (e <- mt.exprs) {
        generateExprCode(e)(ch, variables)
        e.getType match {
          case TUnit => { }
          case _ => { ch << POP }
        }
      }

      generateExprCode(mt.retExpr)(ch, variables)

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
          generateExprCode(lhs)
          generateExprCode(rhs)
          ch << If_ICmpLt(label1) << Ldc(0) << Goto(label2) <<
            Label(label1) << Ldc(1) << Label(label2)
        }
        case Equals(lhs, rhs) => {
          lhs.getType match {
            case TInt | TBoolean => {
              val label1 = ch.getFreshLabel("equals")
              val label2 = ch.getFreshLabel("not_equals")
              generateExprCode(lhs)
              generateExprCode(rhs)
              ch << If_ICmpEq(label1) << Ldc(0) << Goto(label2) <<
              Label(label1) << Ldc(1) << Label(label2)
            }
            case TIntArray => {
              sys.error("IntArray comparison is not implemented yet")
            }
            case TString => {
              sys.error("String comparison is not implemented yet")
            }
            case TUnit => {
              sys.error("Unit comparison is not implemented yet")
            }
            case TClass(_) => {
              sys.error("Class comparison is not implemented yet")
            }
            case _ => {
              sys.error("Tried to match something unexpected in an equals expression")
            }
          }
        }
        case ArrayRead(arr, index) => {

        }
        case ArrayLength(arr) => {
          generateExprCode(arr)
          ch << ARRAYLENGTH
        }
        case MethodCall(obj, meth, args) => {
          ch << Label(ch.getFreshLabel(obj + "." + meth + "(" + args + ")"))
          val retType = meth.asInstanceOf[Identifier].getSymbol.getType
          generateExprCode(obj)
          args foreach { a => generateExprCode(a) }
          val methodSignature = invokeVirtualMethodSig(args, retType)
          ch << InvokeVirtual(getTypeStringOfExprTree(obj
            .asInstanceOf[Identifier]), meth.value, methodSignature) <<
            Label("EndOf-" + obj + "." + meth + "(" + args + ")")
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
          generateExprCode(size)
          ch << NewArray("I")
        }
        case New(tpe) => {
          println(tpe)
          ch << DefaultNew(typeString(tpe))
        }
        case Not(tpe) => {
          generateExprCode(tpe)
          val label1 = ch.getFreshLabel("not-return-1")
          val label2 = ch.getFreshLabel("not-return-0")
          ch << Ldc(1) << If_ICmpNe(label1) << Ldc(0) << Goto(label2) <<
            Label(label1) << Ldc(1) << Label(label2)
        }
        case Block(exprs) => {
          for (b <- exprs) {
            generateExprCode(b)
            b.getType match {
              case TUnit => { }
              case _ => { ch << POP }
            }
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
          ch << Ldc(1) << If_ICmpEq(label2) << Goto(label3) << Label(label2)
          generateExprCode(body)
          ch << Goto(label1) << Label(label3)
        }
        case Println(value) => {
          ch << Label(ch.getFreshLabel("Println(" + value.toString + ")"))
          ch << GetStatic("java/lang/System", "out", "Ljava/io/PrintStream;")
          generateExprCode(value)
          ch << InvokeVirtual("java/io/PrintStream", "println", "(Ljava/lang/String;)V")
          ch << Label(ch.getFreshLabel("EndOf-Println(" + value.toString + ")"))
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
          ch << Label(ch.getFreshLabel("strOf-" + expr))
          expr.getType match {
            case TInt | TBoolean => {
              ch << DefaultNew("java/lang/StringBuilder")
              generateExprCode(expr)
              ch << InvokeVirtual("java/lang/StringBuilder", "append", s"(${getTypeStringOfType(expr.getType)})Ljava/lang/StringBuilder;") <<
                InvokeVirtual("java/lang/StringBuilder", "toString", "()Ljava/lang/String;")
            }
            case _ => sys.error("Strof does not support " + expr.getType)
          }
          ch << Label(ch.getFreshLabel("strOf-" + expr))
        }
      }
    }

    val outDir = ctx.outDir.map(_.getPath+"/").getOrElse("./")

    val f = new java.io.File(outDir)
    if (!f.exists()) {
      f.mkdir()
    }

    val sourceName = ctx.files.head.getName


    generateClassFile(sourceName, mainClassDecl, outDir)

    // output code
    prog.classes foreach {
      ct => generateClassFile(sourceName, ct, outDir)
    }

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
        value
      }
      case _ => {
        sys.error(retType + " has no type!")
      }
    }
  }
  def getTypeStringOfExprTree(e: ExprTree): String = {
    e.getType match {
      case TInt => "I"
      case TBoolean => "B"
      case TString => "Ljava/lang/String"
      case TUnit => "V"
      case TIntArray => "[I"
      case TClass(cs) => cs.name.toString
      case _ => sys.error("Unexpected type: " + e.getType)
    }
  }
  def getTypeStringOfType(t: Type): String = {
    t match {
      case TInt => "I"
      case TBoolean => "Z"
      case TString => "Ljava/lang/String"
      case TUnit => "V"
      case TIntArray => "[I"
      case TClass(cs) => cs.name.toString
      case _ => sys.error("Unexpected type: " + t)
    }
  }

  def invokeVirtualMethodSig(args: List[ExprTree], retType: Type): String = {
    var sig = "("
    for (arg <- args) {
      sig += getTypeStringOfType(arg.getType)
    }
    sig += ")"
    sig += getTypeStringOfType(retType)
    sig
  }

  def parameterString(args: List[Formal]): String = {
    var paramStr = "";
    for (arg <- args) {
      paramStr = paramStr.concat(typeString(arg.tpe))
    }
    paramStr
  }
}
