package webapp

/**
 * Created by bshlegeris on 2/21/15.
 */
import scala.scalajs.js.JSApp

import ast._

object WebInterface extends JSApp {
  def main(): Unit = {
    val function = new FunctionDefinition("whatever", Nil, Map("x" -> 1), Nil)
    println(function.toAssembly(Nil).mkString("\n"))
  }
}
