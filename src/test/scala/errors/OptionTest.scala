package errors

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers


class OptionTest extends AnyFreeSpec with Matchers {

  "An Option" - {
    "should map and increment an int" in {
      val opt: Option[Int] = Some(4)
      opt.map((x) => x + 1) should be(Some(5))
    }

    "GetOrElse should return 5" in {
      val opt: Option[Int] = Some(5)
      opt.getOrElse(0) should be(5)
    }

    "GetOrElse should return 0" in {
      val opt: Option[Int] = None
      opt.getOrElse(0) should be(0)
    }

    "flatmap should return None" in {
      val opt: Option[Int] = None
      opt.flatMap((x) => Some(x + 1)) should be(None)
    }

    "flatMap should return Some(5)" in {
      val opt: Option[Int] = Some(4)
      opt.flatMap((x) => (Some(x + 1))) should be(Some(5))
    }

    "OrElse should return Some(5)" in {
      val opt: Option[Int] = None
      opt.orElse(Some(5)) should be(Some(5))
    }

    "OrElse should return Some(4)" in {
      val opt: Option[Int] = Some(4)
      opt.orElse(Some(5)) should be(Some(4))
    }

    "filter should return None" in {
      val opt: Option[Int] = Some(3)
      opt.filter((x) => x > 3) should be (None)
    }

    "Filter should return Some(4" in {
      val opt: Option[Int] = Some(4)
      opt.filter((x) => x > 3) should be (Some(4))
    }


  }

}
