package assembler

/**
 * Created by bshlegeris on 2/20/15.
 */
import ast._



object AssemblyMaker {
  def separateIntoBlocks(instrs: List[IntermediateInstruction]): List[Block] = {
    var output = List[Block]()
    var currentList = List[IntermediateInstruction]()
    var labelName = "starting-block-" + Counter.getCounter().toString

    var saving = true

    for(line <- instrs) {
      line match {
        case LabelInter(name) => {
          saving = true
          output = output :+ new Block(labelName, currentList)
          labelName = name
          currentList = List()
        }
        case ReturnVoidInter | JumpInter(_) | ReturnWithValInter(_) => {
          if (saving) {
            currentList = currentList :+ line
            saving = false
          }
        }
        case _ => {
          if (saving) {
            currentList = currentList :+ line
          }
        }
      }
    }

    output :+ new Block(labelName, currentList)
  }
}
