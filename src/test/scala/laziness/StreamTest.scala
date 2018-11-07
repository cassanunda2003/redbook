package laziness

import org.scalatest.{FreeSpec, Matchers}

class StreamTest extends FreeSpec with Matchers {
  "Stream" - {
    "Should give a list when using to list" in {
      val st : Stream[Int] = Stream(1,2,3,4)
      st.toList should be (List(1,2,3,4))
    }
  }

}
