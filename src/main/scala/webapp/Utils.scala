package webapp

import scala.scalajs.js.annotation.JSExport

/**
 * Created by bshlegeris on 2/21/15.
 */

@JSExport
object Utils {
  @JSExport
  def some[A](x: A) = Some(x)

  @JSExport
  def none[A]: Option[Any] = None

  @JSExport
  def nil[A] = Nil

  @JSExport
  def cons[A](a: A, b: List[A]) = a +: b

  @JSExport
  def emptyMap = Map()
}
