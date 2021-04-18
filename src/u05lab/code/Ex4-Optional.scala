package u05lab.code

import scala.collection.immutable.List

object Ex4Optional {
  def sequence[A](a: List[Option[A]]): Option[List[A]] = {
    a.foldLeft(Some(List.empty[A]): Option[List[A]])((som: Option[List[A]], elem: Option[A])=> (som, elem) match {
      case (Some(l), Some(a)) => Some( l :+ a)
      case _ => None
    })
  }
}

