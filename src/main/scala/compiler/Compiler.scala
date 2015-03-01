package compiler

import scala.util.Try

import assembler._
import ast._


case class Compiler(functions: List[FunctionDefinition]) {
  def toIntermediate(): Try[List[IntermediateInstruction]] = {
    Try(IntermediateKeyholeOptimizer.optimize(functions.flatMap((function) => {
        AssemblyMaker.separateIntoBlocks(function.toIntermediate()).flatMap(_.code)
      })))
  }

  def toAssembly(): Try[List[Assembly]] = {
    Try(functions.flatMap(compileFunctionToAssembly(_, Nil)))
  }

  def compileFunctionToAssembly(function: FunctionDefinition, globals: List[String]): List[Assembly] = {
    val lol = function.vars.values.toList.asInstanceOf[List[Int]].sum

    val mainBody = (for (block <- function.blocks) yield { new
        BlockAssembler(block,
          function.localsMap,
          globals,
          Some(function.returnPosition),
          function.localVars.length - function.vars.size + lol)
      .assemble() }).flatten

    val optimizedMainBody = AssemblyKeyholeOptimizer.optimize(mainBody)

    optimizedMainBody.last match {
      case ASM_Return => ASM_Label(function.name) +: mainBody
      case _ => ASM_Label(function.name) +: mainBody :+ ASM_Return
    }
  }
}
