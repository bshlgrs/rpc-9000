package compiler

import assembler.{ASM_Label, ASM_Return, BlockAssembler, Assembly}
import ast._

/**
 * Created by bshlegeris on 2/20/15.
 */
case class Compiler(functions: List[FunctionDefinition]) {
  def toIntermediate(): List[IntermediateInstruction] = {
    Nil
//    IntermediateKeyholeOptimizer.optimize(functions.flatMap(_.toIntermediate()))
  }

  def toAssembly(): List[Assembly] = {
    functions.flatMap(compileFunctionToAssembly(_, Nil))
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

    mainBody.last match {
      case ASM_Return => ASM_Label(function.name) +: mainBody
      case _ => ASM_Label(function.name) +: mainBody :+ ASM_Return
    }
  }
}
