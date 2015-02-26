package compiler

import assembler.{ASM_Return, Assembly}
import ast.{ReturnVoidInter, IntermediateInstruction}
import utils._

/**
 * Created by bshlegeris on 2/25/15.
 */

object IntermediateKeyholeOptimizer {
  val identityOptimization = (zipper: Zipper[IntermediateInstruction]) => {
    List(zipper.item)
  }

  type Optimization = (Zipper[IntermediateInstruction] => List[IntermediateInstruction])

  val removeConsecutiveReturnStatements : Optimization = (zipper) => {
    (zipper.prev.headOption, zipper.item) match {
      case (Some(ReturnVoidInter), ReturnVoidInter) => List()
      case _ => List(zipper.item)
    }
  }

  val optimizations = List(removeConsecutiveReturnStatements)

  def optimize(list: List[IntermediateInstruction]): List[IntermediateInstruction] = {
    optimizations.foldLeft(list) ((currentList, function) => {
      Util.mapWithContext(currentList, function)
    })
  }
}

object AssemblyKeyholeOptimizer {
  type Optimization = (Zipper[Assembly] => List[Assembly])

  val removeConsecutiveReturnStatements : Optimization = (zipper) => {
    (zipper.prev.headOption, zipper.item) match {
      case (Some(ASM_Return), ASM_Return) => List()
      case _ => List(zipper.item)
    }
  }

  val optimizations = List(removeConsecutiveReturnStatements)

  def optimize(list: List[Assembly]): List[Assembly] = {
    optimizations.foldLeft(list) ((currentList, function) => {
      currentList ++ Util.mapWithContext(currentList, function)
    })
  }
}
