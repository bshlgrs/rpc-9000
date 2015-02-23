package compiler

import assembler.Assembly
import ast._

/**
 * Created by bshlegeris on 2/20/15.
 */
case class Compiler(functions: List[FunctionDefinition]) {
  def toIntermediate(): List[IntermediateInstruction] = {
    functions.flatMap(_.toIntermediate())
  }

  def toAssembly(): List[Assembly] = {
    functions.flatMap(_.toAssembly(Nil))
  }
}
