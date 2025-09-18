package part4typeclasses

import cats.Monad

import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, Future}

object Semigroupals {

  trait MySemigroupal[F[_]] {
    def product[A, B](fa: F[A], fb: F[B]): F[(A, B)]
  }

  import cats.Semigroupal
  import cats.instances.option.* // implicit Semigroupal[Option]

  val optionSemigroupal = Semigroupal[Option]
  val aTupledSemigroupal = optionSemigroupal.product(Some(123), Some("a string")) // Some((123, "a string"))
  val aNoneTupled = optionSemigroupal.product(Some(123), None) // None

  import cats.instances.future.* // implicit Semigroupal[Future]

  implicit val ec: ExecutionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(8))
  val aTupledFuture = Semigroupal[Future].product(Future("the meaning of life"), Future(42)) // Future(("the meaning of life", 42))

//  import cats.instances.list.* // Monad[List]

  val aTupledList = Semigroupal[List].product(List(1, 2), List("a", "b"))

  def productWithMonads[F[_], A, B](fa: F[A], fb: F[B])(implicit monad: Monad[F]): F[(A, B)] =
    monad.flatMap(fa)(a => monad.map(fb)(b => (a, b)))

  import cats.syntax.flatMap.*
  import cats.syntax.functor.*

  def productWithMonads2[F[_], A, B](fa: F[A], fb: F[B])(implicit monad: Monad[F]): F[(A, B)] =
    for {
      a <- fa
      b <- fb
    } yield (a, b)

  // Monads extends SEMIGROUPALS

  // example: Validated

  import cats.data.Validated

  type ErrorsOr[T] = Validated[List[String], T]

  val validatedSemigroupal = Semigroupal[ErrorsOr] //  requires the implicit Semigroup[List[_]]

  val invalidsCombination = validatedSemigroupal.product(
    Validated.invalid(List("Something wrong", "Something else wrong")),
    Validated.invalid(List("This can't be right"))
  )

  type EitherErrorsOr[T] = Either[List[String], T]

  import cats.instances.either.*

  val eitherSemigroupal = Semigroupal[EitherErrorsOr]

  val eitherCombination = eitherSemigroupal.product(
    Left(List("Something wrong", "Something else wrong")),
    Left(List("This can't be right"))
  )

  //

  implicit object SemigroupalZip extends Semigroupal[List] {
    override def product[A, B](fa: List[A], fb: List[B]): List[(A, B)] =
      fa.zip(fb)
  }

  val zipListSemigroupal: Semigroupal[List] = new Semigroupal[List] {
    override def product[A, B](listA: List[A], listB: List[B]): List[(A, B)] =
      listA.zip(listB)
  }


  def main(args: Array[String]): Unit = {
    println(invalidsCombination)
    println(eitherCombination)
    println(zipListSemigroupal.product(List(1,2), List("a", "b")))
  }

}
