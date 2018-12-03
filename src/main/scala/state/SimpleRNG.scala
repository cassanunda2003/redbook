package state

trait RNG {
  def nextInt: (Int, RNG)
  def nonNegativeInt(rng: RNG): ( Int, RNG)
}

case class SimpleRNG(seed: Long) extends RNG {
  def nextInt:(Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }

  def nonNegativeInt(rng: RNG): (Int, RNG) =
    rng.nextInt match {
      case (x, r) if x < 0 && x > Int.MinValue => (x * -1, r)
      case (x, r) if x == Int.MinValue => (0, r)
        case(x, r) => (x, r)
    }


  
}
