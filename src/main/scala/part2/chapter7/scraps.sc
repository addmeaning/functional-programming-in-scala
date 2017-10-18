import java.util.concurrent._


type Par[A] = ExecutorService => Future[A]


object Par {
  def unit[A](a: A): Par[A] = _ => UnitFuture(a)

  private case class UnitFuture[A](get: A) extends Future[A] {
    override def cancel(mayInterruptIfRunning: Boolean) = false

    override def isCancelled = false

    override def isDone = true

    override def get(timeout: Long, unit: TimeUnit) = get
  }

  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] = (es: ExecutorService) => {
    val af = a(es)
    val bf = b(es)
    UnitFuture(f(af.get(), bf.get))
  }

  def fork[A](a: => Par[A]): Par[A] = // This is the simplest and most natural implementation of `fork`, but there are some problems with it--for one, the outer `Callable` will block waiting for the "inner" task to complete. Since this blocking occupies a thread in our thread pool, or whatever resource backs the `ExecutorService`, this implies that we're losing out on some potential parallelism. Essentially, we're using two threads when one should suffice. This is a symptom of a more serious problem with the implementation, and we will discuss this later in the chapter.
    es => es.submit(new Callable[A] {
      def call = a(es).get
    })

  def delay[A](a: => Par[A]): Par[A] = es => a(es)

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  def asyncF[A, B](f: A => B): A => Par[B] = a => lazyUnit(f(a))

  def sequence[A](ps: List[Par[A]]): Par[List[A]] =
    ps.foldRight[Par[List[A]]](unit(List()))((head, tail) => map2(head, tail)(_ :: _))

  def sequenceRight[A](ps: List[Par[A]]): Par[List[A]] = ps match {
    case Nil => unit(Nil)
    case head :: tail => map2(head, fork(sequenceRight(tail)))(_ :: _)
  }

  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = {

    val pars: List[Par[List[A]]] = as.map(asyncF((a: A) => if (f(a))List(a) else List()))
    map(sequenceRight(pars))(_.flatten)
  }

  def parMap[A, B](ps: List[A])(f: A => B): Par[List[B]] = fork {
    val fbs: List[Par[B]] = ps.map(asyncF(f))
    sequence(fbs)
  }

  def map[A, B](pa: Par[A])(f: A => B): Par[B] =
    map2(pa, unit(()))((a, _) => f(a))


  def equals[A](es: ExecutorService)(left: Par[A], right: Par[A]) = left(es).get == right(es).get
}


def sum(ints: IndexedSeq[Int]): Par[Int] =
  if (ints.size <= 1)
    Par.unit(ints.headOption getOrElse 0)
  else {
    val (l, r) = ints.splitAt(ints.length / 2)
    Par.map2(sum(l), sum(r))(_ + _)
  }


val a = Par.lazyUnit(42 + 1)
val S = Executors.newFixedThreadPool(1)
println(Par.equals(S)(a, Par.fork(a)))
//
//map(y)(id) == y | map(map(y)(g))(f) = map(y) (f compose g) ////f(g(x))
//map(y)(f) == f(y)
//map(map(y)(id))(f) = f(y)
//map(map(y)(g))(f) = f(g(y))
//map(map(y)(g))(f) = map(y)(f compose g)