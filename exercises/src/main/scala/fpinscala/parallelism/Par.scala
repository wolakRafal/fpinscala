package fpinscala.parallelism

import java.util.concurrent._

object Par {
  type Par[A] = ExecutorService => Future[A]

  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

  def unit[A](a: A): Par[A] = (es:ExecutorService) => UnitFuture(a)

  case class UnitFuture[A](get: A) extends Future[A] {
    override def cancel(mayInterruptIfRunning: Boolean): Boolean = false
    override def isCancelled: Boolean = false
    override def get(timeout: Long, unit: TimeUnit): A = get
    override def isDone: Boolean = true
  }

  def fork[A](a: => Par[A]): Par[A] = es => es.submit(new Callable[A] {
    override def call(): A = a(es).get // this will cause a deadlock in some ,(because we block thread on get)
                                       // implementation of executorService (with fixed thread pool size)
  })

  // EXC 1. map2 as primitive
  def map2[A,B,C](a: Par[A], b: Par[B])(f: (A,B) => C): Par[C] = es => {
    val af = a(es)
    val bf = b(es)
    UnitFuture(f(af.get,bf.get))
  }

  // map as primitive
  def map[A, B](pa: Par[A])(f: A => B): Par[B] = es => {
    val fa = pa(es)
    UnitFuture(f(fa.get))
  }

  // another example of map as primitive
  def _map_[A, B](pa: Par[A])(f: A => B): Par[B] = es => UnitFuture(f(pa(es).get))

  def async[A](a: => A): Par[A] = fork(unit(a))

//  EXERCISE 4: using async, write a function to convert any function A => B
//  to one that evaluates its result asynchronously:
  def asyncF[A, B](f: A => B): A => Par[B] = a => async(f(a)) // or fork(unit(f(a)))

  def sortPar_map2(l: Par[List[Int]]): Par[List[Int]] = map2(l, unit(()))((l, _) => l.sorted)
  def sortPar(l: Par[List[Int]]): Par[List[Int]] = map(l)(_.sorted)


//  ... consider it, map2 is actually doing two   things
//    —it is creating a parallel computation that waits for the result of two other
//  computations it is combining their results using some and then function.
// We could split this into two functions, product and map:
//  EXERCISE 5 (optional): Implement product and map as primitives, then
//  define map2 in terms of them.
  def product[A, B](fa: Par[A], fb: Par[B]): Par[(A, B)] =
    es => UnitFuture((fa(es).get, fb(es).get))
  // def map(...) = map is already implemented

  // map2 in terms of map and product
  def map2_product[A, B, C](pa: Par[A], pb: Par[B])(f: ((A, B)) => C): Par[C] =
    map(product(pa, pb))(f)

  // map in terms of map2
  def map_derived[A, B](pa: Par[A])(f: A => B): Par[B] =
    map2(pa, unit(()))((a, _) => f(a))
//
//  EXERCISE 6: Note that we could always just write parMap as a new
//      primitive. See if you can implement it this way. Remember that Par[A] is simply
//  an alias for ExecutorService => Future[A].
//  EXERCISE 7 (hard): Let's write this function, typically called sequence. No
//  additional primitives are required.

  def sequence_simple[A](l: List[Par[A]]): Par[List[A]] = {
    l.foldRight(unit(List[A]()))((pa, acc) => map2(pa, acc)(_ :: _))
  }

  def sequenceBalanced[A](as: IndexedSeq[Par[A]]): Par[IndexedSeq[A]] = fork {
    if (as.isEmpty) unit(Vector())
    else if (as.length == 1) map(as.head)(a => Vector(a))
    else {
      val (l,r) = as.splitAt(as.length / 2)
      map2(sequenceBalanced(l),sequenceBalanced(r))(_++_)
    }
  }

  // much faster
  def sequence[A](l: List[Par[A]]): Par[List[A]] = map(sequenceBalanced(l.toIndexedSeq))(_.toList)

  def parMap[A, B](l: List[A])(f: A => B): Par[List[B]] = fork {
    val fbs = l.map(asyncF(f))
    //... sequence
    sequence(fbs)
  }

  // my parFilter
  def parFilter[A](l: List[A])(p: A => Boolean): Par[List[A]] =
    map(parFilterBalanced(l.toIndexedSeq)(p))(_.toList)

  // parFilter from book - check if this is better
  def parFilter2[A](l: List[A])(f: A => Boolean): Par[List[A]] = {
    val pars: List[Par[List[A]]] =
      l.map(asyncF((a: A) => if (f(a)) List(a) else List()))
    map(sequence(pars))(_.flatten) // convenience method on
                                  // `List` for concatenating a list of lists
  }

  def parFilterBalanced[A](seq: IndexedSeq[A])(p: A => Boolean): Par[IndexedSeq[A]] = fork {
    if(seq.isEmpty) unit(Vector())
    else if (seq.length < 10) unit(seq.filter(p))
    else {
      val (l,r) = seq.splitAt(seq.length/2)
      map2(parFilterBalanced(l)(p),parFilterBalanced(r)(p))(_ ++ _)
    }
  }

  // Here are some ideas to try:
//  Is there a more general version of the parallel summation function we wrote at the
//  beginning of this chapter? Try using it to find the maximum value of an IndexedSeq in
//  parallel.
  def max(seq: IndexedSeq[Int]): Par[Int] = fork {
    if (seq.isEmpty) unit(throw new IllegalArgumentException("Max of empty List"))
    else if (seq.length < 10) unit(seq.max)
    else {
      val (l ,r) = seq.splitAt(seq.length / 2)
      map2(max(l), max(r))(IndexedSeq(_,_).max)
    }
  }

//  Write a function that takes a list of paragraphs (a List[String]), and returns the total
//    number of words across all paragraphs, in parallel. Generalize this function as much as
//    possible.
  def countWords(l: List[String]): Par[Int] = countWordsBalanced(l.toIndexedSeq)

  def countWordsBalanced(seq: IndexedSeq[String]): Par[Int] = {
    if (seq.isEmpty) unit(0)
    else if (seq.length < 5) unit(seq.foldRight(0)((paragraph, counter) => counter + paragraph.split("\\s").length))
    else {
      val (l,r) = seq.splitAt(seq.length / 2)
      map2(countWordsBalanced(l), countWordsBalanced(r))(_ + _)
    }
  }
//    Implement map3, map4, and map5, in terms of map2.

  def map3[A, B, C, D](pa: Par[A], pb: Par[B], pc: Par[C])(f: (A, B, C) => D) = map2(pa, pb)((a, b) => map2(pc, unit(()))((c, _) => f(a, b, c)))

  def map4[A, B, C, D, E](pa: Par[A], pb: Par[B], pc: Par[C], pd: Par[D])(f: (A, B, C, D) => E) = map2(pa, pb)((a, b) => map2(pc, pd)((c, d) => f(a, b, c, d)))

  def map5[A, B, C, D, E, F](pa: Par[A], pb: Par[B], pc: Par[C], pd: Par[D], pe: Par[E])(f: (A, B, C, D, E) => F) =
    map2(pa, pb)((a, b) => map2(pc, pd)((c, d) => map2(pe, unit(()))((e, _) => f(a, b, c, d, e))))

}

object Examples {
  import Par._
  def sum(ints: IndexedSeq[Int]): Par[Int] = // `IndexedSeq` is a superclass of random-access sequences like `Vector` in the standard library. Unlike lists, these sequences provide an efficient `splitAt` method for dividing them into two parts at a particular index.
    if (ints.size <= 1)
      Par.unit(ints.headOption getOrElse 0) // `headOption` is a method defined on all collections in Scala. We saw this function in chapter 3.
    else { 
      val (l,r) = ints.splitAt(ints.length/2) // Divide the sequence in half using the `splitAt` function.
      val sumL: Par[Int] = sum(l)
      val sumR: Par[Int] = sum(r)

      Par.map2(sumL,sumR)(_ + _) // Recursively sum both halves and add the results together.
    }

}

/*
object Par {
  type Par[A] = ExecutorService => Future[A]

  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

  def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a) // `unit` is represented as a function that returns a `UnitFuture`, which is a simple implementation of `Future` that just wraps a constant value. It doesn't use the `ExecutorService` at all. It's always done and can't be cancelled. Its `get` method simply returns the value that we gave it.

  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone = true
    def get(timeout: Long, units: TimeUnit) = get
    def isCancelled = false
    def cancel(evenIfRunning: Boolean): Boolean = false
  }

  def map2[A,B,C](a: Par[A], b: Par[B])(f: (A,B) => C): Par[C] = // `map2` doesn't evaluate the call to `f` in a separate logical thread, in accord with our design choice of having `fork` be the sole function in the API for controlling parallelism. We can always do `fork(map2(a,b)(f))` if we want the evaluation of `f` to occur in a separate thread.
    (es: ExecutorService) => {
      val af = a(es)
      val bf = b(es)
      UnitFuture(f(af.get, bf.get)) // This implementation of `map2` does _not_ respect timeouts, and eagerly waits for the returned futures. This means that even if you have passed in "forked" arguments, using this map2 on them will make them wait. It simply passes the `ExecutorService` on to both `Par` values, waits for the results of the Futures `af` and `bf`, applies `f` to them, and wraps them in a `UnitFuture`. In order to respect timeouts, we'd need a new `Future` implementation that records the amount of time spent evaluating `af`, then subtracts that time from the available time allocated for evaluating `bf`.
    }

  def fork[A](a: => Par[A]): Par[A] = // This is the simplest and most natural implementation of `fork`, but there are some problems with it--for one, the outer `Callable` will block waiting for the "inner" task to complete. Since this blocking occupies a thread in our thread pool, or whatever resource backs the `ExecutorService`, this implies that we're losing out on some potential parallelism. Essentially, we're using two threads when one should suffice. This is a symptom of a more serious problem with the implementation, and we will discuss this later in the chapter.
    es => es.submit(new Callable[A] {
      def call = a(es).get
    })

  def map[A,B](pa: Par[A])(f: A => B): Par[B] =
    map2(pa, unit(()))((a,_) => f(a))

  def sortPar(parList: Par[List[Int]]) = map(parList)(_.sorted)

  def equal[A](e: ExecutorService)(p: Par[A], p2: Par[A]): Boolean =
    p(e).get == p2(e).get

  def delay[A](fa: => Par[A]): Par[A] =
    es => fa(es)

  def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    es =>
      if (run(es)(cond).get) t(es) // Notice we are blocking on the result of `cond`.
      else f(es)

  /* Gives us infix syntax for `Par`. */
  implicit def toParOps[A](p: Par[A]): ParOps[A] = new ParOps(p)

  class ParOps[A](p: Par[A]) {


  }
*/
