package part2abstractMath

import cats.syntax.applicative.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*

import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, Future}

object Monads {
  // lists
  val numbersList = List(1, 2, 3)
  val charsList = List('a', 'b', 'c')
  val newList = numbersList.flatMap(x => charsList.map(y => (x, y)))

  val newList2 = for {
    x <- numbersList
    y <- charsList
  } yield (x, y)

  // options
  val numberOption = Option(2)
  val charOption = Option('d')

  val combinationsOpt = numberOption.flatMap(x => charOption.map(y => (x, y)))
  val combinationsOpt2 = for {
    x <- numberOption
    y <- charOption
  } yield (x, y)

  // futures
  implicit val ec: ExecutionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(8))
  val numberFuture = Future(42)
  val charFuture = Future('Z')

  val combinationFutures = numberFuture.flatMap(x => charFuture.map(y => (x, y)))
  val combinationFutures2 = for {
    x <- numberFuture
    y <- charFuture
  } yield (x, y)

  /*
    Pattern
     - wrapping a value into a monadic value
     - the flatMap mechanism

     MONADS
   */

  trait MyMonad[M[_]] {
    def pure[A](value: A): M[A]

    def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B]

    def map[A, B](ma: M[A])(f: A => B): M[B] =
      flatMap(ma)(x => pure(f(x)))
  }

  //Cats monad

  import cats.Monad
  import cats.instances.option.* // implicit Monad[Option]

  val optionMonad = Monad[Option]
  val anOption = optionMonad.pure(4)
  val aTransformedOption = optionMonad.flatMap(anOption)(x => if (x % 3 == 0) Some(x + 1) else None) // None

  import cats.instances.list.*

  val listMonad = Monad[List]

  val aList = listMonad.pure(3) // List(3)
  val aTransformedList = listMonad.flatMap(aList)(x => List(x, x + 1)) // List(4,5)

  import cats.instances.future.*

  val futureMonad = Monad[Future]

  val aFuture = futureMonad.pure(25) // required an implicit execution context
  val aTransformedFuture = futureMonad.flatMap(aFuture)(x => Future(x + 1)) // Success(87)

  // specialized api
  def getPairsList(numbers: List[Int], chars: List[Char]): List[(Int, Char)] = numbers.flatMap(x => chars.map(y => (x, y)))

  def getPairsOption(number: Option[Int], char: Option[Char]): Option[(Int, Char)] = number.flatMap(x => char.map(y => (x, y)))

  def getPairsFuture(number: Future[Int], char: Future[Char]): Future[(Int, Char)] = number.flatMap(x => char.map(y => (x, y)))

  // generalize
  def getPairs[M[_], A, B](ma: M[A], mb: M[B])(implicit monad: Monad[M]): M[(A, B)] =
    monad.flatMap(ma)(a => monad.map(mb)(b => (a, b)))

  // extension methods  - weirder imports: pure and flatMap

  val oneOption = 1.pure[Option] // implicit Monad[Option] will be used ---> Some(1)
  val oneList = 1.pure[List] // List(1)

  val oneOptionTransform = oneOption.flatMap(x => (x + 1).pure[Option])

  // monads extend functors

  val oneOptionMapped = Monad[Option].map(Option(2))(x => x + 1)
  val oneOptionMapped2 = oneOption.map(_ + 2)

  // for-comprehensions
  val composedOptionFor = for {
    one <- 1.pure[Option]
    two <- 2.pure[Option]
  } yield one + two

  def getPairs2[M[_] : Monad, A, B](ma: M[A], mb: M[B]): M[(A, B)] =
    for {
      a <- ma
      b <- mb
    } yield (a, b)
  //    Monad[M].flatMap(ma)(a => Monad[M].map(mb)(b => (a, b)))

  def getPairs3[M[_] : Monad, A, B](ma: M[A], mb: M[B]): M[(A, B)] =
    ma.flatMap(x => mb.map(y => (x, y)))

  def main(args: Array[String]): Unit = {
    println(getPairs(numbersList, charsList))
    println(getPairs(numberOption, charOption))
    println(getPairs(numberFuture, charFuture).foreach(println))
    println(oneOptionTransform)
  }
}
