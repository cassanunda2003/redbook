package list

import errors.Option
import errors.Some
import errors.None

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class ListSpec extends AnyFreeSpec with Matchers {

  "A List" - {
    "should sum a list of ints " in {
      val l: list.List[Int] = Cons(1, Cons(2, Cons(3, list.Nil)))
      list.List.sum(l) should be (6)
    }

    "should show 0 for NIl list" in {
      val l: list.List[Int] = list.Nil
      List.sum(l) should be (0)
    }

    "List syntax should work " in {
      val l: list.List[Int] = list.List(1,2,3,4)
      list.List.sum(l) should be (10)

    }

    "Pattern matching should work " in {
      val x = List(1,2,3,4,5) match {
        case Cons(x, Cons(2, Cons(4, _))) => x
        case Nil => 42
        case Cons(x, Cons(y,Cons(3, Cons(4,_)))) => x + y
        case Cons(h, t) => h + List.sum(t)
        case _ => 101
      }

      x should be (3)
    }

    "Tail should work " in {
      val x = List(1,2,3,4)
      val tail = list.List.tail(x)
      println(tail)
      assert(list.List.tail(x) == List(2,3,4))
    }

    "Set head should set head to 9" in {
      val x = List(1,2,3,4)
      list.List.setHead(x, 9) should be (List(9,2,3,4))
    }

    "Drop elements should work" in {
      val x = List(1,2,3,4)
      list.List.drop(x, 2) should be (List(3,4))
    }
    "Drop elements should work with one item" in {
      val x = List(1)
      list.List.drop(x, 1) should be (Nil)
    }
    "Drop elements should work with large n" in {
      val x = List(1,2,3,4)
      list.List.drop(x, 8) should be (Nil)
    }

    "Append should work " in {
      val x1 = List(1,2,3)
      val x2 = List(4,5,6)
      list.List.append(x1,x2) should be (List(1,2,3,4,5,6))
    }

    "Int should return start of List" in {
      val l = List(1,2,4)
      list.List.init(l) should be (List(1,2))

    }

    "Sum two should sum ints" in {
      list.List.sum2(List(1,2,3,5,6,7,8)) should be (32)
    }

    "Product should multiply list " in {
      list.List.product(List(2,2)) should be (4)
    }

    "Sum three should sum ints" in {
      list.List.sum3(List(1,2,3,5,6,7,8)) should be (32)
    }

    "Product3 should multiply list " in {
      list.List.product3(List(2,2)) should be (4)
    }

    "Test list cons " in {
      list.List.test(List(1,2,3), Nil) should be (List(1,2,3))
    }

    "Large list now works with foldleft" in {
      def returnLargeList(): List[Int] = {
        var lst: List[Int] = Nil
        for(x <- 1 to 10000)
          lst = List.append(List(x), lst)
        lst
      }
      List.sum3(returnLargeList()) should be (50005000)
    }

    "Use foldLeft to sum an integer list" in {
      val list = List(1,2,3)
      List.foldLeft(list, 0)((x,y) => x + y) should be (6)
    }

    "reverse a list using a fold" in {
      List.reverse(List(1,2,3)) should be (List(3,2,1))
    }

    "append2 should append lists " in {
      List.append2(List(1,2,3),List(4,5,6)) should be (List(1,2,3,4,5,6))
    }

    "Concatenate list of lists" in {
      List.concatenate(List(List(1,2,3),List(4,5,6))) should be (List(1,2,3,4,5,6))
    }
    "Concatenate list of lists 3" in {
      List.concatenate(List(List(1,2),List(3,4),List(5,6))) should be (List(1,2,3,4,5,6))
    }

    "Add one to list of integgers" in {
      List.addOne(List(1,2,3)) should be (List(2,3,4))
    }

    "Convert list of double to string" in {
      List.convertDoubleToString(List(1.0,2.9,4.9)) should be (List("1.0","2.9","4.9"))
    }

    "Convert list of double to string using map" in {
      List.map[Double,String](List(1.0,2.9,4.9))((x)=>x.toString) should be (List("1.0","2.9","4.9"))
    }

    "Filter even" in {
      List.filter(List(1,2,3,4,5,6))(x=>x%2!=0) should be (List(1,3,5))
    }

    "flatmapFilter even" in {
      List.flatMapFilter(List(1,2,3,4,5,6))(x=>x%2!=0) should be (List(1,3,5))
    }

    "Flatmap should work" in {
      List.flatMap(List(1,2,3))(i => List(i,i)) should be (List(1,1,2,2,3,3))
    }

    "Add two int lists " in {
      List.addTwoList(List(1,2,3),List(4,5,6)) should be (List(5,7,9))
    }

    "Zip with " in {
      List.zipWith(List(2,3),List(2,3))((x, y) => x * y) should be (List(4,9))
    }

    "Has subsequence" in {
      List.hasSubsequence(List(1,3,4,5,6), List(3,4,5)) should be (true)
    }
    "Has subsequence false" in {
      List.hasSubsequence(List(9,9,9,9,9), List(7)) should be (false)
    }

    "Sequence should return list of 1,2,3" in {

        List.sequence(List(Some(1),Some(2),Some(3))) should be (Some(List(1,2,3)))
    }
    "Sequence should return list of 1,3" in {

        List.sequence(List(Some(1),None,Some(3))) should be (Some(List(1,3)))
    }

    "meanEither should return error for empty list" in {
      List.meanEither(Seq[Double]()) should be (Left("Error"))
    }

    "meanEither should return 2 " in {
      List.meanEither(Seq(1,2.0,3)) should be (Right(2.0))
    }
  }

}

