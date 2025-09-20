package part4typeclasses

import cats.{Applicative, Apply}

object WeakerMonads {
  trait MyFlatMap[M[_]] extends Apply[M] {
    def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B]

    def ap[A, B](wf: M[A => B])(wa: M[A]): M[B] =
      flatMap(wa)(a => map(wf)(f => f(a)))
  }

  trait MyMonad[M[_]] extends Applicative[M] with MyFlatMap[M] {
    def pure[A](value: A): M[A]

    override def map[A, B](ma: M[A])(f: A => B): M[B] =
      flatMap(ma)(x => pure(f(x)))
  }

  import cats.FlatMap
  import cats.syntax.flatMap.*
  import cats.syntax.functor.*

  def getPairs[M[_] : FlatMap, A, B](numbers: M[A], chars: M[B]): M[(A, B)] =
    for {
      n <- numbers
      c <- chars
    } yield (n, c)

  def main(args: Array[String]): Unit = {

  }
}
