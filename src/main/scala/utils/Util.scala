package utils

/**
 * Created by bshlegeris on 2/24/15.
 */
object Util {
  def mapWithContext[A, B](list: List[A], f: Zipper[A] => List[B]): List[B] = {
    list.foldLeft((List[B](), List[A](), list)) {
      (tuple, item) => {
        val (resultSoFar, previousItems, nextItems) = tuple
        (resultSoFar ++ f(Zipper(item, previousItems, nextItems.drop(1))),
          item +: previousItems,
          nextItems.drop(1))
      }
    }._1
  }
}

case class Zipper[A](item: A, prev: List[A], next: List[A])
