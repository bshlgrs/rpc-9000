package webapp

/**
 * Created by bshlegeris on 2/21/15.
 */

import scala.Any
import scala.scalajs.js
import scala.scalajs.js._
import scala.scalajs.js.annotation.JSExport

import ast._

@JSExport
object WebInterface extends JSApp {
  def main(): Unit = {
    val function = new FunctionDefinition("whatever", Nil, Map("x" -> 1), Nil)
    println(function.toAssembly(Nil).mkString("\n"))
  }

  @JSExport
  def compile(expr: Expr): String = {
    expr.toString
  }

  @JSExport
  def wrapExpr(ast: js.Dictionary[Any]): Expr = {
    def getStr(name: String) = ast(name).asInstanceOf[String]
    def get(name: String) = ast(name).asInstanceOf[js.Dictionary[Any]]
    def getList(name: String) = ast(name).asInstanceOf[js.Array[js.Dictionary[Any]]]

    ast("type") match {
      case "Lit" => Lit(ast("value").asInstanceOf[String].toInt)
      case "BinOp" => BinOp(wrapBinOperator(getStr("op")), wrapExpr(get("lhs")), wrapExpr(get("rhs")))
      case "Var" => Var(getStr("name"))
      case "Load" => Load(wrapExpr(get("expr")))
      case "FunctionCall" => FunctionCall(getStr("name"), wrapExprs(getList("args")))
      case "IfExpression" => ???
      case "StringLiteral" => ???
      case "PointerToName" => ???
    }
  }

  def wrapStatement(ast: js.Dictionary[Any]): Statement = {
    def getStr(name: String) = ast(name).asInstanceOf[String]
    def get(name: String) = ast(name).asInstanceOf[js.Dictionary[Any]]
    def getList(name: String) = ast(name).asInstanceOf[js.Array[js.Dictionary[Any]]]

    ast("type") match {
      case "Assignment" => Assignment(getStr("name"), wrapExpr(get("rhs")))
      case "Return" => if (ast.keys.toList.contains("expr")) {
        Return(Some(wrapExpr(get("expr"))))
      } else {
        Return(None)
      }
      case _ => ???
    }
  }

  def wrapExprs(asts: js.Array[js.Dictionary[Any]]): List[Expr] = {
    asts.toList.map(wrapExpr)
  }

  def wrapBinOperator(op: String): BinaryOperator = op match {
    case "+" => AddOp
    case "*" => MulOp
    case "-" => SubOp
    case "/" => DivOp
    case "%" => ModOp
  }
}
