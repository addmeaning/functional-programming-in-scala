//val x = List(1,2,3,4,5) match {
//  case Cons(x, Cons(2, Cons(4, _))) => x
//  case Nil => 42
//  case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
//  case Cons(h, t) => h + sum(t)
//  case _ => 101
//}
//
//def sum(ints: List[Int]): Int = ints match {
//  case Nil => 0
//  case Cons(x,xs) => x + sum(xs)
//}
//
//case object Nil extends List[Nothing]
//case class Cons[+A](override val head: A, override val tail: List[A]) extends List[A]

def startsWith[A](sup: List[A], sub: List[A]): Boolean = (sup, sub) match {
  case (_, Nil) => true
  case (head::tail, h::t) if head == h => startsWith(tail, t)
  case _ => false
}

def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = sup match {
  case Nil => sub == Nil
  case _ if startsWith(sup, sub) => true
  case _::tail => startsWith(tail, sub)
}