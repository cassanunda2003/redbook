package errors

import errors.Try
import org.scalatest.{FreeSpec, Matchers}

class TryTest extends FreeSpec with Matchers {
  "Try" - {
    "should use try to provide None of fail" in {

      Try.Try(1/0) should be (None)
    }
    "Should returm some(3)" in {
      Try.Try(6/2) should be (Some(3))
    }

    "map2 should return None" in {
      Try.map2(Some(1), None)((a,b) => a+b) should be (None)
    }

    "map2 should return Some(3)" in {
      Try.map2(Some(1),Some(2))((a,b) => a+b) should be (Some(3))
    }

    "map2book should return None" in {
      Try.map2book(Some(1), None)((a,b) => a+b) should be (None)
    }

    "map2book should return Some(3)" in {
      Try.map2book(Some(1),Some(2))((a,b) => a+b) should be (Some(3))
    }
  }

}
