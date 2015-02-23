package webapp

import scala.scalajs.js
import scala.scalajs.js.Dictionary

/**
 * Created by bshlegeris on 2/22/15.
 */
object Parser extends js.Object {
  def parse(str: String): js.Dictionary[Any] = js.native

  def getBody(): String = js.native

  def jsonTree(tree: Any): String = js.native
}
