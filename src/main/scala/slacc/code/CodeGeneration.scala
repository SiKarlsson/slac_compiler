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

  var currentClass: Option[ClassDecl] = None

  def run(ctx: Context)(prog: Program): Unit = {
    import ctx.reporter._

    /** Writes the proper .class file in a given directory. An empty string for dir is equivalent to "./". */
    def generateClassFile(sourceName: String, ct: ClassDecl, dir: String): Unit = {
      var addedMethods = scala.collection.mutable.Set[String]()
      currentClass = Some(ct)
      val classFile = new ClassFile(ct.id.value, None)
      classFile.setSourceFile(sourceName)
      classFile.addDefaultConstructor
      for (vari <- ct.vars) {
          classFile.addField(typeString(vari.tpe), vari.id.value)
      }
      ct.parent match {
        case Some(p) => {
          ct.getSymbol.parent match {
            case Some(pm) => {
              for ((value, varSymbol) <- pm.members) {
                classFile.addField(getTypeStringOfType(varSymbol.getType), value)
              }
            }
            case None => {
              sys.error("ClassDecl has parent but parent has no class symbol")
            }
          }
        }
        case None => { }
      }

      ct.methods foreach {
        meth => {
          if (ct.id.value == "Main") {
            val mainHandler = classFile.addMainMethod.codeHandler
            generateMethodCode(meth)(mainHandler)
          } else {
            val mh: MethodHandler = classFile.addMethod(typeString(meth.retType), meth.id.value, parameterString(meth.args))
            generateMethodCode(meth)(mh.codeHandler)
            addedMethods.add(meth.id.value)
          }
        }
      }


      var parent: Option[ClassDecl] = identToClassDecl(ct.parent, prog)
      while (parent != None) {
        if (parent.get.id.value != "Main") {
          for (meth <- parent.get.methods) {
            if (!addedMethods.contains(meth.id.value)) {
              val mh: MethodHandler = classFile.addMethod(typeString(meth.retType), meth.id.value, parameterString(meth.args))
              generateMethodCode(meth)(mh.codeHandler)
              addedMethods.add(meth.id.value)
            }
          }
        }
        parent = identToClassDecl(parent.get.parent, prog)
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
      var param = 1
      mt.args foreach { mArgs => {
        variables += (mArgs.id.getSymbol -> param)
        param += 1
      }}
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
          val label1 = ch.getFreshLabel("lazy_eval_false")
          val label2 = ch.getFreshLabel("lazy_eval_true")
          generateExprCode(lhs)
          ch << Ldc(1) << If_ICmpNe(label1) << Ldc(1)
          generateExprCode(rhs)
          ch << IAND << Goto(label2) << Label(label1) << Ldc(0) << Label(label2)
        }
        case Or(lhs, rhs) => {
          val label1 = ch.getFreshLabel("lazy_eval_true")
          val label2 = ch.getFreshLabel("lazy_eval_false")
          generateExprCode(lhs)
          ch << Ldc(1) << If_ICmpEq(label1) << Ldc(0)
          generateExprCode(rhs)
          ch << IOR << Goto(label2) << Label(label1) << Ldc(1) << Label(label2)
        }
        case Plus(lhs, rhs) => {
          e.getType match {
            case TString => {
              ch << Label(ch.getFreshLabel("StringConcat-before"))
              val z = ch.getFreshVar("Ljava/lang/StringBuilder;");
              ch << DefaultNew("java/lang/StringBuilder") << AStore(z) << ALoad(z)
              generateExprCode(lhs)
              ch << InvokeVirtual("java/lang/StringBuilder", "append", s"(${typeString(lhs)})Ljava/lang/StringBuilder;")
              generateExprCode(rhs)
              ch << InvokeVirtual("java/lang/StringBuilder",
                "append",
                s"(${typeString(rhs)})Ljava/lang/StringBuilder;") <<
              InvokeVirtual("java/lang/StringBuilder", "toString", "()Ljava/lang/String;")
              ch << Label(ch.getFreshLabel("StringConcat-after"))
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
            case TIntArray | TString | TUnit | TClass(_) => {
              val label1 = ch.getFreshLabel("equals")
              val label2 = ch.getFreshLabel("not_equals")
              generateExprCode(lhs)
              generateExprCode(rhs)
              ch << If_ACmpEq(label1) << Ldc(0) << Goto(label2) <<
                Label(label1) << Ldc(1) << Label(label2)
            }
            case _ => {
              sys.error("Tried to match something unexpected in an equals expression")
            }
          }
        }
        case ArrayRead(arr, index) => {
          generateExprCode(arr)
          generateExprCode(index)
          ch << IALOAD
        }
        case ArrayLength(arr) => {
          generateExprCode(arr)
          ch << ARRAYLENGTH
        }
        case MethodCall(obj, meth: Identifier, args) => {
          ch << Label(ch.getFreshLabel(obj + "." + meth + "(" + args + ")"))
          val retType = meth.getSymbol.getType
          generateExprCode(obj)
          args foreach { a => generateExprCode(a) }
          val methodSignature = invokeVirtualMethodSig(args, retType)
          ch << InvokeVirtual(typeString(obj), meth.value, methodSignature) <<
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
          pushVariable(e.asInstanceOf[Identifier].getSymbol, variables)
        }
        case Self() => {
          ch << ALoad(0)
        }
        case NewIntArray(size) => {
          generateExprCode(size)
          ch << NewArray(10) // 10 = T_INT
        }
        case New(tpe) => {
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
              case TClass(cs) => { }
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
        case Assign(id: Identifier, expr) => {
          // The value to be stored will be on the top of the stack
          generateExprCode(expr)
          val idSym = id.getSymbol
          storeVariable(idSym, variables)
        }
        case ArrayAssign(id: Identifier, index, expr) => {
          ch << ALoad(variables(id.getSymbol))
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
              ch << InvokeVirtual("java/lang/StringBuilder", "append", s"(${typeString(expr)})Ljava/lang/StringBuilder;") <<
                InvokeVirtual("java/lang/StringBuilder", "toString", "()Ljava/lang/String;")
            }
            case _ => sys.error("Strof does not support " + expr.getType)
          }
          ch << Label(ch.getFreshLabel("strOf-" + expr))
        }
        case _ => sys.error("Can't generate bytecode for " + e)
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

  def pushVariable(sym: Symbol, vars: Map[Symbol, Int])(implicit ch: CodeHandler): Unit = {
    vars get sym match {
      case Some(s) => {
        ch << (sym.getType match {
          case TInt => ILoad(s)
          case _ => ALoad(s)
        })
      }
      case None => {
        currentClass match {
          case Some(c) => {
            ch << ALoad(0) << GetField(c.id.value, sym.name, typeString(sym.getType))
          }
          case None => sys.error(s"There is no current class (weirdly enough)")
        }
      }
    }
  }

  def storeVariable(sym: Symbol, vars: Map[Symbol, Int])(implicit ch: CodeHandler): Unit = {
    vars get sym match {
      case Some(s) => {
        ch << (sym.getType match {
          case TInt => IStore(s)
          case _ => AStore(s)
        })
      }
      case None => {
        currentClass match {
          case Some(c) => {
            ch << ALoad(0) << DUP_X1 << POP
            ch << PutField(c.id.value, sym.name, typeString(sym.getType))
          }
          case None => sys.error(s"There is no current class (weirdly enough)")
        }
      }
    }
  }

  def typeString(t: Any): String = {
    t match {
      case tt: TypeTree => {
        tt match {
          case IntType() => "I"
          case StringType() => "Ljava/lang/String;"
          case UnitType() => "V"
          case BooleanType() => "Z"
          case IntArrayType() => "I"
          case Identifier(value) => value
          case _ => sys.error(tt + " has no type!")
        }
      }
      case et: ExprTree => {
        et.getType match {
          case TInt => "I"
          case TBoolean => "Z"
          case TString => "Ljava/lang/String;"
          case TUnit => "V"
          case TIntArray => "[I"
          case TClass(cs) => cs.name.toString
          case _ => sys.error("Unexpected type: " + et.getType)
        }
      }
      case t: Type => {
        t match {
          case TInt => "I"
          case TBoolean => "Z"
          case TString => "Ljava/lang/String;"
          case TUnit => "V"
          case TIntArray => "[I"
          case TClass(cs) => cs.name.toString
          case _ => sys.error("Unexpected type: " + t)
        }
      }
      case _ => sys.error("Don't try to typestring " + t + "...")
    }
  }

  def invokeVirtualMethodSig(args: List[ExprTree], retType: Type): String = {
    var sig = "("
    for (arg <- args) {
      sig += typeString(arg.getType)
    }
    sig += ")"
    sig += typeString(retType)
    sig
  }

  def parameterString(args: List[Formal]): String = {
    var paramStr = "";
    for (arg <- args) {
      paramStr = paramStr.concat(typeString(arg.tpe))
    }
    paramStr
  }

  def identToClassDecl(ident: Option[Identifier], prog: Program): Option[ClassDecl] = {
    ident match {
      case Some(i) => {
        for (classInst <-  prog.classes) {
          if (classInst.id.value == i.value) {
            return Some(classInst)
          }
        }
      }
      case _ => {}
    }
    None
  }
}
