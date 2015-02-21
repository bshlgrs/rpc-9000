package ast

/**
 * Created by bshlegeris on 2/20/15.
 */

object Counter {
  var counter = 0
  def getCounter(): Int = {
    counter = counter + 1
    counter
  }
  def getTempVarName(): String = {
    counter = counter + 1
    "$tmp" + counter.toString
  }
}
