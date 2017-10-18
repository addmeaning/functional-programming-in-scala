package part1.chapter5


sealed trait Stream[+A] {

  def ::[B >: A](prefix: => B): Stream[B] = Stream.cons(prefix, this)

  def :::[B >: A](value: => Stream[B]): Stream[B] = append(value)

  def append[B >: A](value: => Stream[B]): Stream[B] = foldRight(value)((h, t) => Stream.cons(h, t))

  def filter(p: A => Boolean): Stream[A] = foldRight(Stream.empty[A])((h, t) => if (p(h)) Stream.cons(h, t) else t)

  def toList: List[A] =
    this match {
      case Cons(head, tail) => head() :: tail().toList
      case Empty => Nil
    }


  def map[B](f: A => B): Stream[B] = unfold(this) {
    case Cons(h, t) => Some(f(h()), t())
    case Empty => None
  }

  def takeUnfold(n: Int): Stream[A] = unfold((this, n)) {
    case (Cons(h, t), x: Int) if x > 0 => Some(h(), (t(), x - 1))
    case _ => None
  }

  def takeWhileUnfold(p: A => Boolean): Stream[A] = unfold(this) {
    case Cons(h, t) if p(h()) => Some(h(), t())
    case _ => None
  }

  def zipWith[B, C](stream: Stream[B])(f: (A, B) => C): Stream[C] = unfold((this, stream)) {
    case (Empty, _) => None
    case (_, Empty) => None
    case (Cons(h1, t1), Cons(h2, t2)) => Some(f(h1(), h2()), (t1(), t2()))
  }

  def zipAll[B](stream: Stream[B]): Stream[(Option[A], Option[B])] = unfold((this, stream)) {
    case (Empty, Cons(h2, t2)) => Some((None, Some(h2())), (Empty, t2()))
    case (Cons(h1, t1), Empty) => Some((Some(h1()), None), (t1(), Empty))
    case (Cons(h1, t1), Cons(h2, t2)) => Some((Some(h1()), Some(h2())), (t1(), t2()))
    case (Empty, Empty) => None
  }

  def exists(p: A => Boolean): Boolean = this match {
    case Cons(h, t) => p(h()) || t().exists(p)
    case _ => false
  }

  def foldRight[B](acc: => B)(f: (A, => B) => B): B = this match {
    case Cons(h, t) => f(h(), t().foldRight(acc)(f))
    case _ => acc
  }

  def map_fold[B](f: A => B): Stream[B] = foldRight(Stream.empty[B])((h, t) => Stream.cons(f(h), t))

  def flatMap_fold[B](f: A => Stream[B]): Stream[B] = foldRight(Stream.empty[B])((h, t) => f(h) ::: t)

  def existFoldRight(p: A => Boolean): Boolean = foldRight(false) { case (it, acc) => p(it) || acc }


  def forAll(p: A => Boolean): Boolean = this match {
    case Cons(h, t) => p(h()) && t().forAll(p)
    case _ => true
  }

  def forAllFoldRight(p: A => Boolean): Boolean = foldRight(true) { case (it, acc) => p(it) && acc }


  /**
    * Write the function take(n) for returning the first n elements of a Stream, and drop(n) for skipping the first n elements of a Stream.
    *
    * @param n
    * @return
    */

  def take(n: Int): Stream[A] = this match {
    case Cons(head, tail) if n >= 1 => Cons(head, () => tail().take(n - 1))
    case _ => Empty
  }

  def headOption: Option[A] = foldRight[Option[A]](None) { case (it, _) => if (it == Stream.empty) None else Some(it) }

  def takeWhileFoldRight(p: A => Boolean): Stream[A] = foldRight[Stream[A]](Empty) {
    case (it, c) if p(it) => Stream.cons(it, c.takeWhileFoldRight(p))
    case (_, acc) => acc
  }

  def takeWhile_1(p: A => Boolean): Stream[A] = foldRight(Stream.empty[A])((h, t) => if (p(h)) Stream.cons(h, t) else Stream.empty)


  def headOption_1: Option[A] = foldRight[Option[A]](None)((h, _) => Some(h))

  def drop(n: Int): Stream[A] = this match {
    case Cons(_, tail) if n > 0 => tail().drop(n - 1)
    case _ => this
  }

  /**
    * Write the function takeWhile for returning all starting elements of a Stream that match the given predicate.
    * *
    * def takeWhile(p: A => Boolean): Stream[A]
    */
  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(head, tail) if p(head()) => Cons(head, () => tail().takeWhile(p))
    case _ => Empty
  }

  def unfold[B, S](z: S)(f: S => Option[(B, S)]): Stream[B] = Stream.unfold(z)(f)


  def startsWith[B >: A](s: Stream[B]): Boolean = zipAll(s).takeWhile_1(_._2.isDefined).forAll(x => x._1.get == x._2.get)

  ///Stream(Stream(1,2,3), Stream(2,3), Stream(3), Stream())
  def tails: Stream[Stream[A]] = unfold(this) {
    case x: Cons[A] => Some(x, x.t())
    case Empty => None
  }.append(Stream(Stream.empty))

  def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] = foldRight((z, Stream(z)))( (acc, s) => {
    lazy val s1 = s
    val res = f(acc, s1._1)
    (res, Stream.cons(res, s1._2))
  })._2

  def tails_1: Stream[Stream[A]] =
    unfold(this) {
      case Empty => None
      case s => Some((s, s drop 1))
    } append Stream(Stream.empty)

  def hasSubsequence[B >: A](s: Stream[B]): Boolean =
    tails exists (_ startsWith s)
}

case object Empty extends Stream[Nothing] {
  override def toString = "Empty"
}

case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A] {
  override def toString = s"(${h()} :: ${t().toString})"
}


object Stream {
  def cons[A](h: => A, t: => Stream[A]): Stream[A] = {
    lazy val head = h
    lazy val tail = t
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] = if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

  def constant[A](a: A): Stream[A] = {
    lazy val tail: Stream[A] = Cons(() => a, () => tail)
    tail
  }

  def from(a: Int): Stream[Int] = cons(a, from(a + 1))

  def fibs: Stream[Int] = {
    def gen(a: Int, b: Int): Stream[Int] = cons(a, gen(b, a + b))

    gen(0, 1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case Some((h, s)) => cons(h, unfold(s)(f))
    case None => empty
  }

  def fromUnfold(a: Int): Stream[Int] = unfold(a)(x => Some(x, x + 1))

  def constUnfold(a: Int): Stream[Int] = unfold(a)(_ => Some(a, a))


  def fibsViaUnfold: Stream[Int] = unfold((0, 1))(x => Some((x._1, (x._2, x._1 + x._2))))

  def onesViaUnfold: Stream[Int] = constUnfold(1)


}

object Demo extends App {
  val qwe = Cons[Int](() => 1, () => Cons[Int](() => 2, () => Cons[Int](() => 3, () => Empty)))
  //    val qwe2 = Cons[Int](() => 1, () => Empty)
  val qwe2 = Stream(1, 2, 3, 19, 5)
  //  val qwe = Stream(1, 2, 3, 5)
  //  println(qwe2.takeWhile(_ => true))
  println(qwe.tails)
  println(qwe.tails_1)

  println(Stream(1, 2, 3).scanRight(0)(_ + _))
  //  println(Stream.fibs.take(10))
  //  println(Stream.fromUnfold(1).takeWhile(_ < 21))
  //  println(Stream.fromUnfold(1).takeWhileUnfold(_ < 21))
  //  println(Stream.fromUnfold(1).takeWhile_1(_ < 21))
  //  println(Stream.empty.headOption_1)
  //  println(qwe.takeWhileFoldRight(_ < 5))
  //  println(qwe ::: qwe2)

  //  println(qwe2 ::: qwe)

  //  println(qwe2.zipWith(qwe)(_ + _))
  //  println(qwe2.zipAll(qwe))
  //  println(qwe2.exists(_ == 19))
  //  println(qwe2.existFoldRight(_ == 19))
  //  println(qwe2.forAll(_ != 19))
  //  println(qwe2.takeWhileFoldRight(_ != 19))
  //  println(Stream.empty.headOption)
  //  println(Stream().headOption)
  //  println(Stream.empty.forAll((x: Int) => x != 19))

  //  println(Stream.fibsViaUnfold.take(12))
  //  println(qwe2.map(x => x * 2))
}