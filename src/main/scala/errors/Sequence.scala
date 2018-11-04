package errors

trait Sequence {
  def sequencebook[A](lista: List[Option[A]]): Option[List[A]] = lista.foldRight[Option[List[A]]](Some(Nil))((x,y) => Try.map2(x,y)(_ :: _))

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
    a match {
      case Nil => Some(Nil)
      case h::t => Try.map2(f(h), traverse(t)(f))(_ :: _)
    }


}

object Sequence extends Sequence
