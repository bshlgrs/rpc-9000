package assembler

import ast.IntermediateInstruction

/**
 * Created by bshlegeris on 2/20/15.
 */
class Block(val name: String, val code: List[IntermediateInstruction]) {
  override def toString(): String = {
    "Block " + name + "\n" + code.mkString("\n")
  }

  val varsMentioned: List[String] = code.map(x => x.allVars()).flatten.distinct

  // def shittyAllocation(): List[Assembly] = throw new Exception("not implemented yet")
}
