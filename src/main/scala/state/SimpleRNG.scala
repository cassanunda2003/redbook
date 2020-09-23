package state

trait RNG {
  type Rand[+A] = RNG => (A, RNG)

  def nextInt: (Int, RNG)
  def nonNegativeInt(rng: RNG): ( Int, RNG)
}

class SimpleRNG(seed: Long) extends RNG {
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

  def double(rng: RNG): (Double, RNG) = {
    val (i, r) = nonNegativeInt(rng)
    (i / (Int.MaxValue.toDouble + 1), r)
  }



  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, rng2) = nonNegativeInt(rng)
    val (d, rng3) = double(rng2)
    ((i,d),rng3)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val (d, rng2) = double(rng)
    val (i, rng3) = nonNegativeInt(rng2)
    ((d,i), rng3)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d, rng2) = double(rng)
    val (d2, rng3) = double(rng2)
    val (d3, rng4) = double(rng3)
    ((d,d2,d3), rng4)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    def genList(count: Int, acc: List[Int], rng: RNG): (List[Int], RNG) = 
      if (count <= 0) (acc, rng)
      else {
        val (i, rng2) = nonNegativeInt(rng)
        genList(count -1, i :: acc, rng2)
      }
      genList(count, List(), rng)
    }

    val int: Rand[Int] = _.nextInt

    def unit[A](a: A): Rand[A] = rng => (a, rng)

    def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
      rng => {
        val (a, rng2) = s(rng)
        (f(a), rng2)
      }
    
    def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = 
        rng => {
          val (a, rng2) = f(rng)
          g(a)(rng2)
        }
   
    def fmap[A, B](s: Rand[A])(f: A => B): Rand[B] = 
        flatMap(s)(x => unit(f(x)))

    def fmap2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
      flatMap(ra) { b => 
        flatMap(rb) {
           x => unit(f(b, x))
        }
      }
    }

    def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
        rng => {
          val (a, rng2) = ra(rng)
          val (b, rng3) = rb(rng2)
          (f(a, b), rng3)
      }
    }

  val doubleElegant: Rand[Double] = 
      map(nonNegativeInt)( i => i / (Int.MaxValue.toDouble + 1))

   
  val doubleElegant2: Rand[Double] = 
      fmap(nonNegativeInt)( i => i / (Int.MaxValue.toDouble + 1))   

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] = 
    map2(ra, rb)((_, _))

  def both2[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] = 
    fmap2(ra, rb)((_, _))

  
  val randIntDouble: Rand[(Int, Double)] = both(int, doubleElegant)

  val randIntDouble2: Rand[(Int, Double)] = both2(int, doubleElegant)


  def nonNegativeEven: Rand[Int] = map(nonNegativeInt)(i => i - i % 2)


  val randDoubleInt: Rand[(Double, Int)] =
    both(double, int)


    def nonNegativeLessThan(n: Int): Rand[Int] =    
     flatMap(nonNegativeInt) { i =>
      val mod = i % n
      if (i + (n-1) - mod >= 0) unit(mod) else nonNegativeLessThan(n)
    }
}

object SimpleRNG {

    def apply(seed: Long) = new SimpleRNG(seed)

}