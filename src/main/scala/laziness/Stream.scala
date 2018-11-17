package laziness

import Stream._

sealed trait Stream[+A] {
  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, t) => Some(h())
  }
  //Book code sometimes uses mutable objects within local functions
  def toListFast: List[A] = {
    val buf = new collection.mutable.ListBuffer[A]
    @annotation.tailrec
    def go(s: Stream[A]): List[A] = s match {
      case Cons(h,t) =>
        buf += h()
        go(t())
      case _ => buf.toList
    }
    go(this)
  }

  def toList: List[A] ={
      def innerList(acc: List[A], st: Stream[A]): List[A] = {
        st match {
          case Empty => acc
          case Cons(h, t) => innerList(h()::acc, t())
        }
      }
    innerList(Nil, this).reverse
  }

  def drop(n: Int): Stream[A] = {
    def innerStream(x: Int, s: Stream[A]): Stream[A] = {
      s match {
        case Empty => Empty
        case (Cons(h,t)) if x <= 0 => Cons(h,t)
        case (Cons(h,t)) if x > 0 => innerStream(x-1,t())
      }
    }
    innerStream(n,this)
  }
  //Fails as the stream is reverse stream but the values are correct
  def takeMyImpl(n: Int): Stream[A] = {
    def inner(x: Int, is: Stream[A], ps: Stream[A]): Stream[A] = {
      ps match  {
        case Empty => is
        case (Cons(h,t)) if x == 0 => is
        case (Cons(h,t)) if x > 0 => inner(x-1,Cons(h,() => is),t())
      }
    }
    inner(n,Empty,this)
  }

  //Recursive had trouble as to why I would meed the import statement but it is necessary to include the methods
  //in the object cons and empty
  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 1 => cons(h(), t().take(n - 1))
    case Cons(h, _) if n == 1 => cons(h(), empty)
    case _ => empty
  }

  def takeWhile(f: A => Boolean):Stream[A] = this match {
    case Cons(h,t) if(f(h())) => cons(h(), t() takeWhile(f))
    case _ => Empty
  }

  def takeWhileRight(f: A => Boolean):Stream[A] = foldRight(Empty: Stream[A])((a, b) => if(f(a)) cons(a, b) else empty)

  def existsRec(p: A => Boolean): Boolean = this match {
    case Cons(h, t) => p(h()) || t().exists(p)
    case _ => false
  }

  //book exists
  // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.
  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b)

  def foldRight[B](z: => B)(f: (A, => B) => B): B =
      this match {
         case Cons(h,t) => f(h(), t().foldRight(z)(f))
          case _ => z
    }



  def forAll(p: A => Boolean): Boolean = foldRight(true)((a, b) => p(a) && b)

  def headOptionRight(): Option[A] = foldRight(None: Option[A])(
    (a, _) => Some(a)
  )

  def map[B](f: A => B): Stream[B] = foldRight(Empty: Stream[B])((a, b) => cons(f(a), b))

  def filter(f: A => Boolean): Stream[A] = foldRight(Empty: Stream[A])((a, b) => if(f(a)) cons(a,b) else b)

  def append[B>:A](s1: Stream[B]): Stream[B] = foldRight(s1)((x,b) => cons(x,b))

  def flatMap[B](f: A => Stream[B]): Stream[B] = foldRight(Empty: Stream[B])((a,b) =>f(a) append b)
}

case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream  {

  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(()=>head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if(as.isEmpty) empty else cons(as.head, apply(as.tail: _*))


}