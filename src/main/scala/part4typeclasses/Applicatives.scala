package part4typeclasses

object Applicatives {
  // Applicatives = Functors + the pure method

  import cats.Applicative
  import cats.instances.list.*

  val listApplicative = Applicative[List]
  val aList = listApplicative.pure(2) // List(2)

  import cats.instances.option.*

  val optionApplicative = Applicative[Option]
  val anOption = optionApplicative.pure(2) // Some(2)

  // pure extension method

  import cats.syntax.applicative.*

  val aSweetList = 2.pure[List] // List(2)
  val aSweetOption = 2.pure[Option] // Some(2)


  // Monad extend Applicatives
  // Applicatives extend Functors

  import cats.data.Validated

  type ErrorsOr[T] = Validated[List[String], T]
  val aValidValue: ErrorsOr[Int] = Validated.valid(43) // "pure"
  val aModifiedValidated: ErrorsOr[Int] = aValidValue.map(_ + 1) // map

  val validatedApplicative = Applicative[ErrorsOr]

  //  def ap[F[_], B, T](ff: F[B => T])(fa: F[B]): F[T] = ???

  def productWithApplicatives[F[_], A, B](fa: F[A], fb: F[B])(implicit applicative: Applicative[F]): F[(A, B)] = {
    val functionWrapper: F[B => (A, B)] = applicative.map(fa)(a => (b: B) => (a, b))
    applicative.ap(functionWrapper)(fb)
  }

  // Applicatives extend Semigroupal

  def main(args: Array[String]): Unit = {

  }
}
