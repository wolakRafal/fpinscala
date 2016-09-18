package repetition

import java.util.concurrent.{ExecutorService, Future, TimeUnit}

import fpinscala.monads.Functor
import fpinscala.parallelism.Par.Par

object MonadAgain {

  trait Monad[M[_]] extends Functor[M] {
    def unit[A](a: => A): M[A]

    def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B]

//    def map[A, B](f : A => B): M[B] = flatMap(a => unit(f(a)))
    def map[A,B](ma: M[A])(f: A => B): M[B] = flatMap(ma)(a => unit(f(a)))


  }

//  Par, Parser, Option, Stream, and List.
  val parMonad  = new Monad[Par] {
    override def unit[A](a: => A): Par[A] = (es: ExecutorService) => new Future[A] {
      override def isCancelled: Boolean = false
      override def get(): A = a
      override def get(timeout: Long, unit: TimeUnit): A = a
      override def cancel(mayInterruptIfRunning: Boolean): Boolean = false
      override def isDone: Boolean = true
    }

    override def flatMap[A, B](ma: Par[A])(f: (A) => Par[B]): Par[B] = (es: ExecutorService) => f(ma(es).get())(es)
  }


//  Not Safe , fix NPE when epmty stream
  val streamMonad = new Monad[Stream] {
    override def unit[A](a: => A): Stream[A] = Stream(a)

    override def flatMap[A, B](sa: Stream[A])(f: (A) => Stream[B]): Stream[B] = {
      val prefix = f(sa.head)
      prefix #::: flatMap(sa.tail)(f)
    }
  }

}
