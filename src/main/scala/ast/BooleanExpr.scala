package ast

/**
 * Created by bshlegeris on 2/20/15.
 */

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

case class BooleanBinOp(op: BoolBinOperator, lhs: Expr, rhs: Expr) extends BooleanExpr {
  override def toIntermediate(thenLabel: String, elseLabel: String): List[IntermediateInstruction] = {
    op match {
      case GreaterOrEqual => return (
        BooleanBinOp(GreaterThan, BinOp(AddOp, Lit(1), lhs), rhs)
          .toIntermediate(thenLabel, elseLabel)
        )
      case _ => ()
    }
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

    (lhsCode ::: rhsCode ::: comparisonInstrs)
  }
}

case class AndExpr(lhs: BooleanExpr, rhs: BooleanExpr) extends BooleanExpr {
  override def toIntermediate(thenLabel: String, elseLabel: String): List[IntermediateInstruction] = {
    val myLabel = "and-"+Counter.counter
    val lhsCode = lhs.toIntermediate(myLabel, elseLabel)
    val rhsCode = rhs.toIntermediate(thenLabel, elseLabel)
    return (lhsCode ::: LabelInter(myLabel) +: rhsCode)
  }
}

case class OrExpr(lhs: BooleanExpr, rhs: BooleanExpr) extends BooleanExpr {
  override def toIntermediate(thenLabel: String, elseLabel: String): List[IntermediateInstruction] = {
    val myLabel = "and-"+Counter.counter
    val lhsCode = lhs.toIntermediate(thenLabel, myLabel)
    val rhsCode = rhs.toIntermediate(thenLabel, elseLabel)
    return (lhsCode ::: LabelInter(myLabel) +: rhsCode)
  }
}

case class NotExpr(expr: BooleanExpr) extends BooleanExpr {
  override def toIntermediate(thenLabel: String, elseLabel: String) = expr.toIntermediate(elseLabel, thenLabel)
}

sealed abstract class BoolBinOperator {
  override def toString: String = this match {
    case Equals => "=="
    case GreaterThan => ">"
    case GreaterOrEqual => ">="
  }
}

case object Equals extends BoolBinOperator
case object GreaterThan extends BoolBinOperator
case object GreaterOrEqual extends BoolBinOperator
