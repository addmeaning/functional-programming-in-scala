package part1.chapter4

import scala.util.Try

sealed trait Either[+L, +R] {

  def map[Y](f: R => Y): Either[L, Y] = this match {
    case Right(x) => Right(f(x))
    case Left(x) => Left(x)
  }

  def flatMap[LL >: L, Y](f: R => Either[LL, Y]): Either[LL, Y] = this match {
    case Right(x) => f(x)
    case Left(x) => Left(x)
  }

//  def orElse[LL >: L, Y >: R](f: R => Either[LL, Y]): Either[LL, Y] = this match {
//    case Right(x) => Right(x)
//    case Left(x) => f(x)
//  }

  def map2[LL >: L, Y, Z](z: Either[LL, Y])(f: (R, Y) => Z): Either[LL, Z] =
    for {a <- this; y <- z} yield f(a, y)


  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = es match {
    case Nil => Right(Nil)
    case head :: tail => head.flatMap(x => sequence(tail) map (x :: _))
  }

  //  def seqFold[E, A](es: List[Either[E, A]]): Either[E, List[A]] =
  //    es.foldRight(Right(List()))(x => map2(x)(identity))

  def traverse[E, A, B](as: List[A])(
    f: A => Either[E, B]): Either[E, List[B]] = as.foldRight[Either[E,List[B]]](Right(Nil))((a, b) => f(a).map2(b)(_ :: _))

  def seqViaTraverse[E, A](es: List[Either[E, A]]): Either[E, List[A]] = traverse(es)(identity)

}

case class Left[+L](value: L) extends Either[L, Nothing]

case class Right[+R](value: R) extends Either[Nothing, R]


object ErrorHandling {
  def mean(seq: Seq[Double]): Either[String, Double] =
    if (seq.isEmpty) Left("mean of empty list")
    else Right(seq.sum / seq.length)
}


object Demo extends App {
  private val result: Either[String, Double] = ErrorHandling.mean(List(1.0, 2.0, 3.0))
  result match {
    case Left(s) => println(s"exception occurred $s")
    case Right(mean) => println(s"mean equal $mean")
  }
}