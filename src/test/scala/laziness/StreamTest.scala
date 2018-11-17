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
    "takewhile should return stream containing values less than 3" in {
      val st = Stream(1,2,3,4,5,1)
      st.takeWhile(x => x < 3).toList should be (List(1,2))
    }
    "takewhileRight should return stream containing values less than 3" in {
      st.takeWhileRight(x => x < 3).toList should be (List(1,2))
    }

    "takewhileRight should return stream containing values less than 3 but not those after 5" in {
      val st = Stream(1,2,3,4,5,1)
      st.takeWhileRight(x => x < 3).toList should be (List(1,2))
    }
    "exists should return true" in {
      st.exists(_ == 2) should be (true)
    }
    "exists should return false" in {
      st.exists(_ == 9) should be (false)
    }
    "forall should return true" in {
      st.forAll(_ < 9) should be (true)
    }
    "forall should return false" in {
      st.forAll(_ < 2) should be (false)
    }

    "headOptionRight should return Some(1)" in {
      st.headOptionRight should be (Some(1))
    }

    "map should add 1 to each item in stream" in {
      st.map((x) => x + 1).toList should be (List(2,3,4,5))
    }

    "filter should return filtered stream" in {
      st.filter(x => x%2 == 0).toList should be (List(2,4))
    }

    "append should append new stream" in {
      st.append(Stream(5,6)).toList should be (List(1,2,3,4,5,6))
    }

    "flatMap" in {
      st.flatMap(i => Stream(i,i)).toList should be (List(1,1,2,2,3,3,4,4))
    }

    "constant" in {
      st.constant(1).take(5).toList should be (List(1,1,1,1,1))
    }
  }

}
