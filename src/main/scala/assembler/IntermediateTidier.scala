package assembler

import ast._

/**
 * Created by bshlegeris on 2/23/15.
 */
object IntermediateTidier {
  def tidy(stuff: List[IntermediateInstruction]): List[IntermediateInstruction] = {
    val pairs = stuff.zip(stuff.drop(1))

    ???
  }
}
