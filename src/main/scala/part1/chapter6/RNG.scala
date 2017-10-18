package part1.chapter6

import part1.chapter6.RNG.Rand


trait RNG {
  def nextInt: (Int, RNG)


}

case class SimpleRNG(seed: Long) extends RNG {
  override def nextInt: (Int, SimpleRNG) = {
    val newSeed = ((seed + 0x42DL) * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val state = (newSeed >>> 16).toInt
    (state, nextRNG)
  }


}

object RNG {
  type Rand[+A] = RNG => (A, RNG)


  def unit[A](a: A): Rand[A] = rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] = rng => {
    val (res, newRng) = s(rng)
    (f(res), newRng)
  }

  def nonNegativeEven: Rand[Int] = map(nonNegativeInt)(i => i - i % 2)

  def mapDouble: Rand[Double] = map(nonNegativeInt)(i => i / (Int.MaxValue.toDouble + 1))


  def apply(seed: Long): RNG = SimpleRNG(seed)

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (value, nextRNG) = rng.nextInt
    if (value >= 0) (value, nextRNG) else (-value + 1, nextRNG)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (value, nextRNG) = nonNegativeInt(rng)
    (value / (Int.MaxValue.toDouble + 1), nextRNG)
  }

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, nextRNG) = rng.nextInt
    val (d, resultRNG) = double(nextRNG)
    ((i, d), resultRNG)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val ((i, d), resultRNG) = intDouble(rng)
    ((d, i), resultRNG)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d0, rng0) = double(rng)
    val (d1, rng1) = double(rng0)
    val (d2, rng2) = double(rng1)
    ((d0, d1, d2), rng2)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    require(count >= 0, "count can't be less than zero")

    def go(count: Int)(rng: RNG, list: List[Int]): (List[Int], RNG) = {
      val (i, nextRNG) = rng.nextInt
      if (count == 0) (list, rng) else go(count - 1)(nextRNG, i :: list)
    }

    go(count)(rng, List.empty)
  }

  def sequence_1[A](fs: List[Rand[A]]): Rand[List[A]] = fs.foldRight(unit(List.empty[A]))((f, acc) => map2(f, acc)(_ :: _))

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = rng => {
    fs.foldRight((List.empty[A], rng))((it, acc) => {
      val (value, nextRNG) = it(acc._2)
      (value :: acc._1, nextRNG)
    })
  }


  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = rng => {
    val (resA, newRng0) = ra(rng)
    val (resB, newRng1) = rb(newRng0)
    (f(resA, resB), newRng1)
  }

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] =
    map2(ra, rb)((_, _))


  def many[A](count: Int)(rng: RNG, f: Rand[A]): (List[A], RNG) = {
    require(count >= 0, "count can't be less than zero")

    def go(count: Int)(rng: RNG, list: List[A], f: Rand[A]): (List[A], RNG) = {
      val (i, nextRNG) = f(rng)
      if (count == 0) (list, rng) else go(count - 1)(nextRNG, i :: list, f)
    }

    go(count)(rng, List.empty, f)
  }


}

object Demo extends App {
  println(RNG.ints(50)(RNG(0))._1)
  println(RNG.many(50)(RNG(0), RNG.mapDouble)._1)

  val i: Rand[Int] = _.nextInt
}
