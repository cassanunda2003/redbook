package errors

object Try {
  def Try[A](a: => A): Option[A] =
    try Some(a)
    catch { case e: Exception => None }

  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A,B) => C): Option[C] = (a,b) match {
    case (None,_) => None
      case(_,None) => None
      case(Some(x),Some(y)) => Some(f(x,y))
  }

  def map2book[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    a flatMap (aa => b map (bb => f(aa, bb)))

}
