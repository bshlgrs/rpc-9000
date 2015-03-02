package ast

import scala.scalajs.js.annotation.JSExport

import ast.BinOp

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
        op match {
          case Equals => {
            val (instrs, outputVar) = BinOp(SubOp, lhs, rhs).toIntermediate()
            instrs ++ List(JumpNZInter(elseLabel, outputVar),
              JumpInter(thenLabel))
          }

          case GreaterThan => {
            val (instrs, outputVar) = BinOp(SubOp, rhs, lhs).toIntermediate()
            instrs ++ List(JumpNInter(thenLabel, outputVar),
              JumpInter(elseLabel))
          }
          case GreaterOrEqual => throw new Exception("something is hideously wrong")
        }
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
