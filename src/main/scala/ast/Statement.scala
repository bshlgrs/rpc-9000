package ast

import scala.scalajs.js.annotation.JSExport


sealed abstract class Statement {
  override def toString: String = this match {
    case Assignment(name, rhs) => name + " = " + rhs.toString + ";"
    case VoidFunctionCall(name, args) => name + "(" + args.mkString(", ") + ")"
    case IndirectAssignment(lhs, rhs) => "*" + lhs.toString + " = " + rhs.toString+";"
    case IfElse(condition, thenBlock, elseBlock) => "if (" + condition.toString +
      ") { \n"+thenBlock.mkString("\n") + "} \nelse {\n" +
      elseBlock.mkString("\n") + "\n}"
    case While(condition, block) => "while (" + condition.toString + ") {\n" +
      block.mkString("\n") + "\n}"
    case ForLoop(a,b,c,d) => "for ("+a+"; "+b+"; "+c+") {\n"+d.mkString("\n")+"}"
    case Return(thing) => thing match {
      case Some(expr) => "return " + expr.toString + ";"
      case None => "return;"
    }
    case ArrayAssignment(name, stuff) => (name + " = { " + stuff.mkString(", ")
      + " };")
  }

  def toIntermediate(): List[IntermediateInstruction]

  def allExpressions: List[Expr] =  this match {
    case Assignment(_,rhs) => List(rhs)
    case VoidFunctionCall(_,args) => args
    case IndirectAssignment(lhs,rhs) => List(lhs, rhs)
    case IfElse(condition, thenBlock, elseBlock) => (condition.allExpressions
      ::: thenBlock.map{_.allExpressions}.flatten
      ::: elseBlock.map{_.allExpressions}.flatten)
    case While(condition, block) => condition.allExpressions :::
      block.flatMap{_.allExpressions}
    case ForLoop(a,b,c,d) =>
      a.allExpressions ::: b.allExpressions ::: c.allExpressions :::
        d.flatMap{_.allExpressions}
    case Return(Some(x)) => List(x)
    case Return(None) => List()
    case ArrayAssignment(_, stuff) => stuff
  }
}

case class Assignment(name: String, rhs: Expr) extends Statement {
  override def toIntermediate(): List[IntermediateInstruction] = {
    val (exprInters, resultPlace) = rhs.toIntermediate()
    if (exprInters.length == 0)
      return List(CopyInter(resultPlace, name))

    resultPlace match {
      case VOLVar(x) => {
        val changedInters = exprInters.map {_.changeTarget(x, name)}
        CommentInter(this.toString()) +: changedInters
      }
      case VOLLit(n) => {
        List(CommentInter(this.toString()), CopyInter(VOLLit(n), name))
      }
    }
  }
}

case class IndirectAssignment(lhs: Expr, rhs: Expr) extends Statement {
  override def toIntermediate(): List[IntermediateInstruction] = {
    val (lhsInstr, lhsVar) = lhs.toIntermediate()
    val (rhsInstr, rhsVar) = rhs.toIntermediate()
    CommentInter(this.toString()) +: (lhsInstr ::: rhsInstr) :+
      StoreInter(rhsVar, lhsVar)
  }
}

case class IfElse(condition: BooleanExpr,
                  thenBlock: List[Statement],
                  elseBlock: List[Statement]) extends Statement {
  override def toIntermediate(): List[IntermediateInstruction] = {
    val counter = Counter.getCounter()
    val conditionCode = condition.toIntermediate("then-"+counter.toString,
      "else-"+counter.toString) : List[IntermediateInstruction]
    val thenCode = StatementHelper.statementsToIntermediate(thenBlock)
    val elseCode = StatementHelper.statementsToIntermediate(elseBlock)

    List(CommentInter("if (" + condition.toString + ") {")) :::
      conditionCode :::
      List(CommentInter("{")) :::
      List(LabelInter("then-"+counter.toString)) :::
      (thenCode :+
        JumpInter("end-"+counter.toString) :+
        CommentInter("} else {") :+
        LabelInter("else-"+counter.toString)) :::
      (elseCode :+
        LabelInter("end-"+counter.toString)) :::
      List(CommentInter("}"))
  }
}

case class While(condition: BooleanExpr, block: List[Statement]) extends Statement {
  override def toIntermediate(): List[IntermediateInstruction] = {
    val counter = Counter.getCounter()
    val conditionCode = condition.toIntermediate("while-loop-" + counter.toString + "-body",
      "endWhile-"+counter.toString) : List[IntermediateInstruction]

    val blockCode = (for (line <- block) yield line.toIntermediate()).flatten

    List(CommentInter("while ("+ condition.toString + ") {"),
      LabelInter("while-" + counter.toString)) :::
      (conditionCode :+
        LabelInter("while-loop-" + counter.toString + "-body")) :::
      blockCode :::
      List(JumpInter("while-" + counter.toString),
        CommentInter("}"),
        LabelInter("endWhile-"+counter.toString))
  }
}

case class Return(value: Option[Expr]) extends Statement {
  override def toIntermediate(): List[IntermediateInstruction] = value match {
    case None => {
      List(CommentInter(this.toString), ReturnVoidInter)
    }
    case Some(expr) => {
      val (exprCode, returnPlace) = expr.toIntermediate()
      List(CommentInter(this.toString)) ::: exprCode ::: List(ReturnWithValInter(returnPlace))
    }
  }
}

case class VoidFunctionCall(name: String, args: List[Expr]) extends Statement {
  override def toIntermediate(): List[IntermediateInstruction] = {
    val arg_code = for( arg <- args ) yield arg.toIntermediate()
    val code = List.concat(for ((code, varOrLit) <- arg_code) yield code).flatten
    val vars = for ((code, varOrLit) <- arg_code) yield varOrLit
    val callInstruction = CallInter(name, vars, None)

    CommentInter(this.toString()) +: code :+ callInstruction
  }
}

case class ForLoop(instr: Statement, cond: BooleanExpr, iterator: Statement,
                   block: List[Statement]) extends Statement {
  override def toIntermediate() = {
    instr.toIntermediate() ::: While(cond, block :+ iterator).toIntermediate()
  }
}

case class ArrayAssignment(name: String, exprs: List[Expr]) extends Statement {
  override def toIntermediate(): List[IntermediateInstruction] = {
    if (exprs.length == 0) {
      Nil
    } else if (exprs.length == 1) {
      new IndirectAssignment(Var(name), exprs(0)).toIntermediate()
    } else {
      val counter = name+"-"+Counter.getCounter()
      val start = CopyInter(VOLVar(name), counter)
      val blockCode = (for {
        (expr, index) <- exprs.view.zipWithIndex
      } yield {
        val (instrs, resultPlace) = expr.toIntermediate()
        instrs ::: List(StoreInter(resultPlace, VOLVar(counter))) :::
          List(BinOpInter(
            AddOp, VOLVar(counter), VOLLit(1), counter))
      }).toList.flatten

      List(CommentInter(this.toString()), start) ::: blockCode
    }
  }
}

object StatementHelper {
  def statementsToIntermediate(block: List[Statement]): List[IntermediateInstruction] = {
    block.flatMap(_.toIntermediate())
  }
}
