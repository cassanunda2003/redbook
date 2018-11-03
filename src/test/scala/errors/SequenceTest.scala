package errors

import errors.Sequence
import org.scalatest.{FreeSpec, Matchers}

class SequenceTest extends FreeSpec with Matchers {
  "Sequence should return list of 1,2,3" in {

    Sequence.sequencebook(List(Some(1),Some(2),Some(3))) should be (Some(List(1,2,3)))
  }
  "Sequence should return list of 1,3" in {

    Sequence.sequencebook(List(Some(1),None,Some(3))) should be (None)
  }
}
