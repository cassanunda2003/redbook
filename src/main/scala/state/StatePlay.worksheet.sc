// val rng = new scala.util.Random

// val r = rng.nextDouble

// val r2 = rng.nextDouble

// val r3 = rng.nextInt

// val r4 = rng.nextInt(10)

import state._

val rng = SimpleRNG(42)

val (i, rng2) = rng.nextInt

val (i2, rng3) = rng2.nextInt

def wrongRandomPair(rng: RNG): (Int, Int) = {
    val (i1, _) = rng.nextInt
    val (i2, _) = rng.nextInt
    (i1, i2)
} //Returnd the same value

wrongRandomPair(rng)

def rightRandomPair(rng: RNG): ((Int, Int), RNG) = {
    val (i1, rng2) = rng.nextInt
    val (i2, rng3) = rng2.nextInt
    ((i1, i2), rng3)
}

rightRandomPair(rng)

rng.double(rng)

rng.intDouble(rng)

rng.doubleInt(rng)

rng.double3(rng)

rng.ints(3)(rng)

rng.int(rng)

val nonneg = rng.nonNegativeEven

nonneg(rng)

rng.doubleElegant(rng)

rng.doubleElegant2(rng)

rng.randIntDouble(rng)

rng.randIntDouble2(rng)
