package compiler
import ast._

/**
 * Created by bshlegeris on 2/20/15.
 */
object Compiler {
  def main (args: Array[String]) {
    val function = new FunctionDefinition("whatever", Nil, Map("x" -> 1), Nil)
    println(function.toAssembly(Nil).mkString("\n"))
  }
}
