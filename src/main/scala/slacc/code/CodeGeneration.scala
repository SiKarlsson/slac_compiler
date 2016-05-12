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
      val classFile = new ClassFile(ct.id.value + ".class", None)
      classFile.setSourceFile(sourceName)
      ct.methods foreach {
        meth => {
          val mh: MethodHandler = classFile.addMethod(typeString(meth.retType), meth.id.value, parameterString(meth.args))
          generateMethodCode(mh.codeHandler, meth)
        }
      }
      classFile.writeToFile(dir + "/" + ct.id.value + ".class")
    }

    // a mapping from variable symbols to positions in the local variables
    // of the stack frame
    def generateMethodCode(ch: CodeHandler, mt: MethodDecl): Unit = {
      val methSym = mt.getSymbol

      mt.args foreach {
        mArgs => println(mArgs)
      }

      mt.vars foreach {
        mVars => println(mVars)
      }

      mt.exprs foreach {
        mExpr => generateExprCode(ch, mExpr)
      }

      ch << ILoad(1) << IRETURN
      ch.freeze
    }

    def generateExprCode(ch: CodeHandler, e: ExprTree): Unit = {
      e match {
        case And(lhs, rhs) => {

        }
        case Or(lhs, rhs) => {

        }
        case Plus(lhs, rhs) => {

        }
        case Minus(lhs, rhs) => {

        }
        case Times(lhs, rhs) => {

        }
        case Div(lhs, rhs) => {

        }
        case LessThan(lhs, rhs) => {

        }
        case Equals(lhs, rhs) => {

        }
        case ArrayRead(arr, index) => {

        }
        case ArrayLength(arr) => {

        }
        case MethodCall(obj, meth, args) => {

        }
        case IntLit(value) => {

        }
        case StringLit(value) => {

        }
        case True() => {

        }
        case False() => {

        }
        case Identifier(value) => {

        }
        case Self() => {

        }
        case NewIntArray(size) => {

        }
        case New(tpe) => {

        }
        case Not(tpe) => {

        }
        case Block(exprs) => {

        }
        case If(expr, thn, els) => {

        }
        case While(cond, body) => {

        }
        case Println(expr) => {

        }
        case Assign(id, expr) => {

        }
        case ArrayAssign(id, index, expr) => {

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
