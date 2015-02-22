package ast

import scala.scalajs.js.annotation.JSExport

@JSExport
sealed abstract class BooleanExpr {
  override def toString: String = this match {
    case BooleanBinOp(op, e1, e2) => e1.toString + " " + op.toString + " " + e2.toString
    case AndExpr(lhs, rhs) => "("+ lhs.toString + " && " + rhs.toString + ")"
    case OrExpr(lhs, rhs) => "("+ lhs.toString + " || " + rhs.toString + ")"
    case NotExpr(expr) => "!(" + expr.toString + ")"
  }

  def toIntermediate(thenLabel: String, elseLabel: String): List[IntermediateInstruction] = throw new Exception("unimplemented")

  def allExpressions: List[Expr] = this match {
    case BooleanBinOp(_, e1, e2) => List(e1, e2)
    case AndExpr(e1, e2) => e1.allExpressions ::: e2.allExpressions
    case OrExpr(e1, e2) => e1.allExpressions ::: e2.allExpressions
    case NotExpr(e) => e.allExpressions
  }
}

@JSExport
case class BooleanBinOp(op: BoolBinOperator, lhs: Expr, rhs: Expr) extends BooleanExpr {
  override def toIntermediate(thenLabel: String, elseLabel: String): List[IntermediateInstruction] = {
    op match {
      case GreaterOrEqual => {
        BooleanBinOp(GreaterThan, BinOp(AddOp, Lit(1), lhs), rhs).toIntermediate(thenLabel, elseLabel)
      }
      case _ => {
        val (lhsCode, lhsResult) = lhs.toIntermediate()
        val (rhsCode, rhsResult) = rhs.toIntermediate()
        val outputVar = Counter.getTempVarName()

        val comparisonInstrs: List[IntermediateInstruction] = op match {
          case Equals => List(BinOpInter(SubOp, lhsResult, rhsResult, outputVar),
            JumpNZInter(elseLabel, VOLVar(outputVar)),
            JumpInter(thenLabel))

          case GreaterThan => List(BinOpInter(SubOp, rhsResult, lhsResult, outputVar),
            JumpNInter(thenLabel, VOLVar(outputVar)),
            JumpInter(elseLabel))
          case GreaterOrEqual => throw new Exception("something is hideously wrong")
        }

        lhsCode ::: rhsCode ::: comparisonInstrs
      }
    }
  }
}

@JSExport
case class AndExpr(lhs: BooleanExpr, rhs: BooleanExpr) extends BooleanExpr {
  override def toIntermediate(thenLabel: String, elseLabel: String): List[IntermediateInstruction] = {
    val myLabel = "and-"+Counter.counter
    val lhsCode = lhs.toIntermediate(myLabel, elseLabel)
    val rhsCode = rhs.toIntermediate(thenLabel, elseLabel)
    lhsCode ::: LabelInter(myLabel) +: rhsCode
  }
}

@JSExport
case class OrExpr(lhs: BooleanExpr, rhs: BooleanExpr) extends BooleanExpr {
  override def toIntermediate(thenLabel: String, elseLabel: String): List[IntermediateInstruction] = {
    val myLabel = "and-"+Counter.counter
    val lhsCode = lhs.toIntermediate(thenLabel, myLabel)
    val rhsCode = rhs.toIntermediate(thenLabel, elseLabel)
    lhsCode ::: LabelInter(myLabel) +: rhsCode
  }
}

@JSExport
case class NotExpr(expr: BooleanExpr) extends BooleanExpr {
  override def toIntermediate(thenLabel: String, elseLabel: String) = expr.toIntermediate(elseLabel, thenLabel)
}

@JSExport
sealed abstract class BoolBinOperator {
  override def toString: String = this match {
    case Equals => "=="
    case GreaterThan => ">"
    case GreaterOrEqual => ">="
  }
}

@JSExport
case object Equals extends BoolBinOperator
@JSExport
case object GreaterThan extends BoolBinOperator
@JSExport
case object GreaterOrEqual extends BoolBinOperator
