package webapp

import scala.Any
import scala.scalajs.js
import scala.scalajs.js._
import scala.scalajs.js.annotation.JSExport
import compiler.Compiler
import org.scalajs.jquery.{JQuery, jQuery}

import ast._

@JSExport
object WebInterface extends JSApp {
  @JSExport
  def main(): Unit = {
    jQuery("#compile-button").on("click", handleClick _)
  }

  @JSExport
  def handleClick(x: Any): Unit = {
    Counter.reset()
    val input = JsInterface.getBody()
    val ast = JsInterface.parse(input).asInstanceOf[js.Array[js.Dictionary[Any]]]
    jQuery("#ast-output").html(JsInterface.jsonTree(ast))
    val compiler = Compiler(ast.toList.map(wrapFunctionDef))
    jQuery("#intermediate-output").html(s"${compiler.toIntermediate().mkString("\n")}")
    val text = s"<pre>${compiler.toAssembly().mkString("\n")}</pre>"
    jQuery("#assembly-output").html(text)
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
      case "FunctionCall" => FunctionCall(getStr("name"), mapWrap(wrapExpr, getList("args")))
      case "IfExpression" => ???
      case "StringLiteral" => ???
      case "PointerToName" => ???
    }
  }

  def wrapBooleanExpr(ast: js.Dictionary[Any]): BooleanExpr = {
    def getStr(name: String) = ast(name).asInstanceOf[String]
    def get(name: String) = ast(name).asInstanceOf[js.Dictionary[Any]]
    def getList(name: String) = ast(name).asInstanceOf[js.Array[js.Dictionary[Any]]]

    ast("type") match {
      case "BooleanBinOp" => BooleanBinOp(
        wrapBooleanBinOperator(getStr("op")),
        wrapExpr(get("lhs")),
        wrapExpr(get("rhs")))
    }
  }

  def wrapStatement(ast: js.Dictionary[Any]): Statement = {
    def getStr(name: String) = ast(name).asInstanceOf[String]
    def get(name: String) = ast(name).asInstanceOf[js.Dictionary[Any]]
    def getList(name: String) = ast(name).asInstanceOf[js.Array[js.Dictionary[Any]]]

    ast("type") match {
      case "Assignment" => Assignment(getStr("name"), wrapExpr(get("rhs")))
      case "Return" => if (ast.keys.toList.contains("value")) {
        Return(Some(wrapExpr(get("value"))))
      } else {
        Return(None)
      }
      case "IfElse" => {
        IfElse(wrapBooleanExpr(get("condition")), mapWrap(wrapStatement, getList("thenBlock")),
          if (ast.contains("elseBlock"))
            mapWrap(wrapStatement, getList("elseBlock"))
          else
            Nil)
      }
      case _ => {
        println(s"that statement can't be parsed yet: ${JSON.stringify(ast)}")
        ???
      }
    }
  }

  def wrapBinOperator(op: String): BinaryOperator = op match {
    case "+" => AddOp
    case "*" => MulOp
    case "-" => SubOp
    case "/" => DivOp
    case "%" => ModOp
  }

  def wrapBooleanBinOperator(op: String): BoolBinOperator = op match {
    case "==" => Equals
    case ">" => GreaterThan
    case ">=" => GreaterOrEqual
  }

  @JSExport
  def wrapFunctionDef(ast: js.Dictionary[Any]): FunctionDefinition = {
    def getStr(name: String) = ast(name).asInstanceOf[String]
    def get(name: String) = ast(name).asInstanceOf[js.Dictionary[Any]]
    def getList(name: String) = ast(name).asInstanceOf[js.Array[js.Dictionary[Any]]]

    val args = mapWrap(wrapArg, getList("args"))

    new FunctionDefinition(getStr("name"), args, Map(), mapWrap(wrapStatement, getList("body")))
  }

  def mapWrap[A, B](f: A => B, arr: js.Array[A]): List[B] = {
    arr.toList.map(f)
  }

  def wrapArg(ast: js.Dictionary[Any]): (String, CType) = ast("name").asInstanceOf[String] -> IntType
}
