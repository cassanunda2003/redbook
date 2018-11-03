package list

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {

  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(head, tail) => head + sum(tail)
  }

  def apply[A](as: A*): List[A] = {
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
  }

  def tail[A](list: List[A]): List[A] = list match {
    case Nil => Nil
    case Cons(h, t) => t
  }

  def head[A](list: List[A]): Option[A] = list match {
    case Nil => None
    case Cons(h,t) => Some(h)
  }

  def setHead[A](list: List[A], value: A): List[A] = list match {
    case Nil => Nil
    case Cons(h, t) => Cons(value, t)
  }

  def drop[A](list: List[A], elements: Int): List[A] = (list: List[A], elements: Int) match {
    case (Nil, _) => Nil
    case (l, n) if n <= 0 => l
    case (Cons(h, t), n) => drop(t, n - 1)

  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h, t) => Cons(h, append(t, a2))
    }

  def init[A](list: List[A]): List[A] = {
    def inner(acc: List[A], lst: List[A]): List[A] = {
      lst match {
        case Nil => acc
        case Cons(h, Nil) => acc
        case Cons(h, t) => inner(append(acc, List(h)), t)
      }
    }
    inner(Nil, list)
  }

  def foldRightOrg[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) => {
        f(x, foldRightOrg(xs, z)(f))
      }

    }

  def foldRight[A,B](lst: List[A], z: B)(f: (A,B) => B): B =
    foldLeft(reverse(lst),z)((b,a)=>f(a,b))

  def foldRightViaFoldLeft_1[A,B](l: List[A], z: B)(f: (A,B) => B): B =
    foldLeft(l, (b:B) => b)((g,a) => b => g(f(a,b)))(z)


  def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B = {
    def inner(currValue: B, list: List[A]): B = {
      list match {
        case Nil => currValue
        case Cons(x, xs) => inner(f(currValue, x), xs)
      }
    }
    inner(z, as)
  }


  def sum2(list: List[Int]) =
    foldRight(list, 0)((x,y) => x + y)

  def product(list: List[Double]): Double = foldRight(list, 1.0)(_ * _)

  def test(lst: List[Int], lst2: List[Int])
  = foldRight(lst, lst2)(Cons(_,_))

  def sum3(list: List[Int]): Int = foldLeft(list,0)((x,y) => x + y)


  def product3(list: List[Double]): Double = foldLeft(list, 1.0)(_ * _)

  def reverse[A](list: List[A]) = {
    val l: List[A] = Nil
    foldLeft(list, l)((x, y) => Cons(y, x))
  }

  def append2[A](a1: List[A], a2: List[A]): List[A] = {
    foldLeft(List.reverse(a1),a2)((x,y) => Cons(y, x))
  }

  def concatenate[A](lists: List[List[A]]): List[A] = {

    def inner(acc: List[A], lst: List[List[A]]): List[A] = {
      lst match {
        case Nil => acc
        case Cons(x, Nil) => List.append2(acc, x)
        case Cons(x, xs) => inner(List.append2(acc, x), xs)
      }
    }
    val l: List[A] = Nil
    inner(l, lists)
  }

  def addOne(ints: List[Int]): List[Int] = {
    val start: List[Int] = Nil
    foldRight(ints, start)((x, b) => Cons(x+1,b))
  }

  def convertDoubleToString(lst: List[Double]): List[String] =
    foldRight(lst, Nil:List[String])((x,b)=>Cons(x.toString,b))

  def map[A,B](as: List[A])(f: A => B): List[B] = {
    foldRight(as, Nil:List[B])((x,b)=>Cons(f(x),b))
  }

  def filter[A](as: List[A])(f: A => Boolean): List[A]  = {
    val n: List[A] = Nil
    foldRight(as, Nil: List[A])((a, b) => if (f(a)) {
      Cons(a, b)
    } else b)
  }

  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] =
    foldRight(as,Nil:List[B])((a,b)=>append2(f(a),b))

  def flatMapFilter[A](as: List[A])(f: A => Boolean): List[A] = {
    flatMap(as)(x => if(f(x)) List(x) else Nil)
  }

  def addTwoList(as: List[Int], ab: List[Int]): List[Int] = {
    def inner(acc: List[Int], l1: List[Int], l2: List[Int]): List[Int] =
      (l1, l2) match {
        case(Nil,_) => acc
        case (_, Nil) => acc
        case (Cons(x,xs), Cons(y, ys)) => inner(Cons(x+y, acc),xs, ys)
      }

    List.reverse(inner(Nil:List[Int],as,ab))
  }

  def zipWith[A, B](as: List[A], ab: List[A])(f: (A, A) => B): List[B] = {
    def inner(acc: List[B], l1: List[A], l2: List[A]): List[B] =
      (l1, l2) match {
        case(Nil,_) => acc
        case (_, Nil) => acc
        case (Cons(x,xs), Cons(y, ys)) => inner(Cons(f(x,y), acc),xs, ys)
      }
    List.reverse(inner(Nil:List[B],as,ab))
  }

  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
    def sequence(lst1: List[A], lst: List[A], matches: Boolean): Boolean = {
      (lst, lst1, matches) match {
          case(_,_,true) => true
          case (Nil, _,_) => matches
          case (_, Nil,_) => matches
          case(Cons(x,xs), Cons(y,ys),_) => sequence(xs, ys, x==y)
      }
    }

    def checkSequences(bl: Boolean, innerLst: List[A]): Boolean = {
      println("Boolean is " + bl + " List is " + innerLst.toString)
      (bl, innerLst) match {
        case (true, _) =>  true
        case (_, Nil) => bl
        case (_,Cons(x, xs)) => checkSequences(sequence(xs,sub,false), xs)
      }
    }

    if(sequence(sup, sub, false))
      true
    else
      checkSequences(false,sup)

  }
}
