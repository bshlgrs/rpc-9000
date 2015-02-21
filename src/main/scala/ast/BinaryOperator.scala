package ast

/**
 * Created by bshlegeris on 2/20/15.
 */
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

case object AddOp extends BinaryOperator
case object MulOp extends BinaryOperator
case object SubOp extends BinaryOperator
case object DivOp extends BinaryOperator
case object ModOp extends BinaryOperator
