package utils

/**
 * Created by bshlegeris on 2/24/15.
 */
object Util {
  def mapWithContext[A, B](list: List[A], f: (A, List[A], List[A]) => B): List[B] = {
    list.foldLeft((Nil, Nil, list)) ((resultSoFar, previousItems, nextItems), item) => {
      resultSoFar :+ f(resultSoFar, previousItems, nextItems), 

    }._1
  }
}
