package ast

import scala.scalajs.js.annotation.JSExport

@JSExport
sealed abstract class BinaryOperator {
  override def toString: String = this match {
    case AddOp => "+"
    case MulOp => "*"
    case SubOp => "-"
    case DivOp => "/"
    case ModOp => "%"
  }

  def toAssembly: String = this match {
    case AddOp => "add"
    case MulOp => "mult"
    case SubOp => "sub"
    case DivOp => "div"
    case ModOp => "mod"
  }
}

@JSExport
case object AddOp extends BinaryOperator
@JSExport
case object MulOp extends BinaryOperator
@JSExport
case object SubOp extends BinaryOperator
@JSExport
case object DivOp extends BinaryOperator
@JSExport
case object ModOp extends BinaryOperator
