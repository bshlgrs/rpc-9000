package ast

sealed abstract class IntermediateInstruction {
  override def toString: String = this match {
    case BinOpInter(op, in1, in2, target) => "\t"+target +"\t=  "+in1.toString + "  "+op.toString + "  "+in2.toString
    case LoadInter(source, target) => "\t "+target.toString + "\t= * "+source.toString
    case StoreInter(source, target) => "\t* "+target.toString + "\t=  "+source.toString
    case CopyInter(source, target) => "\t "+target.toString + "\t=  "+source.toString
    case LabelInter(label) => label+":"
    case JumpInter(label) => "\tjump "+label
    case JumpZInter(label, sourceVar) => "\tjump "+label+" if 0 == "+sourceVar
    case JumpNZInter(label, sourceVar) => "\tjump "+label+" if 0 != "+sourceVar
    case JumpNInter(label, sourceVar) => "\tjump "+label +" if 0 > "+sourceVar
    case CallInter(name, args, target) => "\t"+target + "= "+name + "("+ args.mkString(", ") + ")"
    case AmpersandInter(name, target) => "\t"+target+"= &" +name
    case PushInter => "\tpush;"
    case PopInter(target) => "\tpop "+target+";"
    case CommentInter(comment) => "; " + comment
    case ReturnVoidInter => "\treturn;"
    case ReturnWithValInter(x) => "\treturn " + x.toString + ";"
    case _ => throw new Exception("unimplemented")
  }

  def changeTarget(oldTarget: String, newTarget: String):IntermediateInstruction = this match {
    case BinOpInter(op, in1, in2, target) => {
      if (target == oldTarget)
        BinOpInter(op, in1, in2, newTarget)
      else
        this
    }
    case LoadInter(source, target) => {
      if (target == oldTarget)
        LoadInter(source, newTarget)
      else
        this
    }
    case CopyInter(source, target) => {
      if (target == oldTarget)
        CopyInter(source, newTarget)
      else
        this
    }
    case PopInter(target) => {
      if (target == oldTarget)
        PopInter(newTarget)
      else
        this
    }

    case CallInter(x,y,Some(n)) => {
      if (n == oldTarget)
        CallInter(x,y,Some(newTarget))
      else
        this
    }
    case AmpersandInter(x,target) => {
      if (target == oldTarget)
        AmpersandInter(x, newTarget)
      else
        this
    }

    case _ => this
  }

  // TODO: it would probably be wise to check all of this stuff very carefully
  def inputVars():List[VarOrLit] = this match {
    case BinOpInter(_, in1, in2, _) => List(in1, in2)
    case LoadInter(a,_) => List(a)
    case StoreInter(a, b) => List(a, b)
    case CopyInter(a, _) => List(a)
    case LabelInter(_) => List()
    case JumpInter(_) => List()
    case JumpZInter(_,a) => List(a)
    case JumpNInter(_,a) => List(a)
    case JumpNZInter(_,a) => List(a)
    case CallInter(_, args, _) => args
    case PopInter(_) => List()
    case PushInter => List()
    case CommentInter(_) => List()
    case ReturnWithValInter(x) => List(x)
    case ReturnVoidInter => List()
    case AmpersandInter(_,_) => List()
  }

  // TODO: I should also check this
  def outputVars():List[String] = this match {
    case BinOpInter(_,_,_,out) => List(out)
    case LoadInter(_,o) => List(o)
    case CopyInter(_,o) => List(o)
    case PopInter(o) => List(o)
    case CallInter(_,_,Some(x)) => List(x)
    case AmpersandInter(_,x) => List(x)
    case _ => List()
  }

  def allVars():List[String] = inputVars.map{_.varListIfVar()}.flatten ::: outputVars

  // This is the place that an instruction might jump to within its own function.
  def jumpTargets(): Option[String] = this match {
    case JumpInter(x) => Some(x)
    case JumpZInter(x,_) => Some(x)
    case JumpNInter(x,_) => Some(x)
    case JumpNZInter(x,_) => Some(x)
    case _ => None
  }
}

case class BinOpInter(op: BinaryOperator, in1: VarOrLit, in2: VarOrLit, targetVar: String) extends IntermediateInstruction
case class LoadInter(sourceVar: VarOrLit, targetVar: String) extends IntermediateInstruction
case class StoreInter(sourceVar: VarOrLit, targetVar: VarOrLit) extends IntermediateInstruction
case class CopyInter(sourceVar: VarOrLit, targetVar: String) extends IntermediateInstruction
case class LabelInter(label: String) extends IntermediateInstruction
case class JumpInter(label: String) extends IntermediateInstruction
case class JumpZInter(label: String, sourceVar: VarOrLit) extends IntermediateInstruction
case class JumpNInter(label: String, sourceVar: VarOrLit) extends IntermediateInstruction
case class JumpNZInter(label: String, sourceVar: VarOrLit) extends IntermediateInstruction
case class CallInter(name: String, args: List[VarOrLit], target:Option[String]) extends IntermediateInstruction
case class AmpersandInter(name: String, target: String) extends IntermediateInstruction
case object PushInter extends IntermediateInstruction
case class PopInter(target: String) extends IntermediateInstruction
case class CommentInter(comment: String) extends IntermediateInstruction
case class ReturnWithValInter(value: VarOrLit) extends IntermediateInstruction
case object ReturnVoidInter extends IntermediateInstruction
