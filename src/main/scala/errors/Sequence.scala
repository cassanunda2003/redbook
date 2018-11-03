package errors

trait Sequence {
  def sequencebook[A](lista: List[Option[A]]): Option[List[A]] = lista.foldRight[Option[List[A]]](Some(Nil))((x,y) => Try.map2(x,y)(_ :: _))
}

object Sequence extends Sequence
