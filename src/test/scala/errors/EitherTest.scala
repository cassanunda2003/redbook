package errors

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers


class EitherTest extends AnyFreeSpec with Matchers {
  val e = new Exception("Error")
  "Either" - {
    "map should return either 2" in {
      val r: Either[Exception, Int] = Right(1)
      r.map((x) => x + 1) should be (Right(2))
    }
    "map should be left" in {
      val l: Either[Exception, Int] = Left(e)
      l.map((x)=>x + 1) should be (Left(e))
    }
    "flatmap should return either 2" in {
      val r: Either[Exception, Int] = Right(1)
      r.flatMap((x) => Right(x + 1)) should be (Right(2))
    }
    "flatmap should be left" in {
      val l: Either[Exception, Int] = Left(e)
      l.flatMap((x)=>Right(x + 1)) should be (Left(e))
    }
    "orElse should return Either Right(2)" in {
      val r: Either[Exception, Int] = Right(2)
      r.orElse(Right(3)) should be (Right(2))
    }
    "orElse should return Either Right(3)" in {
      val l: Either[Exception, Int] = Left(e)
      l.orElse(Right(3)) should be (Right(3))
    }

    "map2 should retrun Right(5)" in {
      val in1: Either[Exception, Int] = Right(1)
      val in2: Either[Exception, Int] = Right(4)
      in1.map2(in2)((x,y) => x + y) should be (Right(5))
    }
   "map2 should retrun Left" in {
      val in1: Either[Exception, Int] = Left(e)
      val in2: Either[Exception, Int] = Right(4)
      in1.map2(in2)((x,y) => x + y) should be (Left(e))
    }
   "map2 should retrun Left again" in {
      val in1: Either[Exception, Int] = Right(1)
      val in2: Either[Exception, Int] = Left(e)
      in1.map2(in2)((x,y) => x + y) should be (Left(e))
    }


  }
}
