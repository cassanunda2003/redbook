package laziness

import org.scalatest.{FreeSpec, Matchers}

class StreamTest extends FreeSpec with Matchers {
  "Stream" - {
    val st : Stream[Int] = Stream(1,2,3,4)
    "Should give a list when using to list" in {
      st.toList should be (List(1,2,3,4))
    }
    "take should produce a Stream of the number of the argument" in {
      st.take(2).toList should be (List(1,2))
    }
    "take 0 should return the empty Stream" in {
      st.take(0).toList should be (List.empty)
    }
    "take with a number greater than than size should return the same stream " in{
      st.take(5).toList should be (List(1,2,3,4))
    }

    "drop should return elements after the position given" in {
      st.drop(2).toList should be (List(3,4))
    }
    "drop 0 return the same stream" in {
      st.drop(0).toList should be (List(1,2,3,4))
    }
    "drop with more elements should return empty stream" in {
      st.drop(5).toList should be (List.empty)
    }
  }

}
