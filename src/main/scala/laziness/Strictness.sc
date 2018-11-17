import laziness._

def if2[A](cond: Boolean, onTrue:  => A, onFalse:  => A): A =
  if (cond) onTrue else onFalse

val a = 33

val c = if2(a>22, 42,  32)

val d = a + c

val ones: Stream[Int] = Stream.cons(1, ones)

ones.take(5).toList

ones.takeWhile(_ == 1)

ones.forAll(_ != 1)