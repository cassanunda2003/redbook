package example


import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class VarianceTest extends AnyFreeSpec with Matchers {

  "Variance" - {
    "mean method should return the arithmetic mean of a list" in {
      Variance.mean(List(1,2.0,3)) should be (Some(2.0))
    }
    "variance method should return the variance of the list" in {
        Variance.variance(List(12, 12, 12, 12, 13.000)) should be (Some(0.16))
    }

    "variance empty sequence should be None" in {
        Variance.variance(List()) should be (None)
    }
  }

}
