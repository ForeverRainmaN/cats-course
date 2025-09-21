package part4typeclasses

import cats.{ApplicativeError, Monad}

import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try

object HandlingErrors {

  trait MyMonadError[M[_], E] extends Monad[M] {
    def raiseError[A](e: E): M[A]
  }

  import cats.MonadError // implicit MonadError

  type ErrorOr[A] = Either[String, A]
  val monadErrorEither = MonadError[ErrorOr, String]
  val success = monadErrorEither.pure(32) // Either[String, Int] == Right(32)
  val failure = monadErrorEither.raiseError[Int]("Something wrong") // Either[String, Int] ==  Left("Something wrong")

  //recover
  val handleError: ErrorOr[Int] = monadErrorEither.handleError(failure) {
    case "Badness" => 44
    case _ => 89
  }
  // recoverWith
  val handledError2: ErrorOr[Int] = monadErrorEither.handleErrorWith(failure) {
    case "Badness" => monadErrorEither.pure(44) // ErrorOr[Int]
    case _ => Left("Something else") // ErrorOr[Int]
  }

  type Test = Either[*, String]
  // "filter"
  val filteredSuccess = monadErrorEither.ensure(success)("Number to small")(_ > 100)

  // Try and Future

  import cats.instances.try_.*

  val exception = new RuntimeException("Really bad")
  val pureExeption: Try[Int] = MonadError[Try, Throwable].raiseError(exception) // Try[Nothing] // Failure(exception)

  import cats.instances.future.*

  implicit val ec: ExecutionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(8))
  MonadError[Future, Throwable].raiseError(exception)

  // applicatives => ApplicativeError

  import cats.data.Validated
  import cats.instances.list.* // implicit Semigroup[List] => ApplicativeError[ErrorsOr, List[String]]

  type ErrorsOr[T] = Validated[List[String], T]
  val appErrorVal = ApplicativeError[ErrorsOr, List[String]]
  // pure, raiseError, handleError, handleErrorWith

  def main(args: Array[String]): Unit = {

  }
}
