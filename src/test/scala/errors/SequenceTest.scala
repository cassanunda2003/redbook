package errors

import org.scalatest.{FreeSpec, Matchers}

class SequenceTest extends FreeSpec with Matchers {
  "Sequence should return list of 1,2,3" in {

    Sequence.sequencebook(List(Some(1),Some(2),Some(3))) should be (Some(List(1,2,3)))
  }
  "Sequence should return list of 1,3" in {

    Sequence.sequencebook(List(Some(1),None,Some(3))) should be (None)
  }

  "traverse should return Some List" in {

    Sequence.traverse(List(1,2,6))((x: Int) => Some(12.0/x)) should be (Some(List(12.0,6.0,2.0)))
  }
  "Sequence should return None" in {

    Sequence.traverse(List(1,0,3))((x: Int) => if (x==0) None else Some(2/x)) should be (None)
  }

}
