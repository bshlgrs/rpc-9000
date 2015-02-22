package ast

sealed abstract class CType {
  def size(): Int
}

case object IntType extends CType {
  def size() = 1
}

case object VoidType extends CType {
  def size() = 0
}

case class PointerType(insideType: CType) {
  def size() = 1
}

case class StructType(name: String, parts: List[(String, CType)]) extends CType {
  def size() = parts.map(_._2.size).sum
}
