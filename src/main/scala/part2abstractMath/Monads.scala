package part2abstractMath

import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, Future}

object Monads {

  // lists
  val numbersList = List(1, 2, 3)
  val charsList = List("a", "b", "c")
  // TODO 1.1
  val combinations = for {
    num <- numbersList
    ch <- charsList
  } yield (num, ch)

  val combinationsv2 = numbersList.flatMap(
    x => charsList.map(y => (x, y))
  )

  val numberOption = Option(2)
  val charOption = Option('d')

  // TODO 1.2
  val combinationOpts = for {
    n <- numberOption
    c <- charOption
  } yield (n, c)

  // futures
  implicit val ec: ExecutionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(8))

  val numberFuture = Future(42)
  val charFuture = Future('z')
  // TODO 1.3
  val combinationFutures = for {
    n <- numberFuture
    c <- charFuture
  } yield (n, c)

  /*
    pattern:
      - wrapping a value into a monadic value
      - the flatMap mechanism

      MONAD
   */

  trait MyMonad[M[_]] {
    def pure[A](value: A): M[A]

    def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B]

    def map[A, B](ma: M[A])(f: A => B): M[B] =
      flatMap(ma)(x => pure(f(x)))
  }

  // Cats monad

  import cats.Monad
  import cats.instances.option.* // Implicit Monad[Option]

  val optionMonad = Monad[Option]
  val anOption = optionMonad.pure(4)
  val aTransformedOption = optionMonad.flatMap(anOption)(x => if (x % 3 == 0) Some(x + 1) else None) // None

  import cats.instances.list.*

  val listMonad = Monad[List]
  val aList = listMonad.pure(3) // List(3)
  val aTransformedList = listMonad.flatMap(aList)(x => List(x, x + 1)) // List(4, 5)

  // TODO 2: use a Monad[Future]

  import cats.instances.future.*

  val futureMonad = Monad[Future]
  val aFuture = futureMonad.pure(43) // require an implicit execution context
  val aTransformedFuture = futureMonad.flatMap(aFuture)(x => Future(x + 44)) // future that will and up with Success(87)

  // specialized API:
  def getPairsList(numbers: List[Int], chars: List[Char]): List[(Int, Char)] =
    numbers.flatMap(n => chars.map(c => (n, c)))

  def getPairsOption(number: Option[Int], char: Option[Char]): Option[(Int, Char)] =
    number.flatMap(n => char.map(c => (n, c)))

  def getPairsFuture(number: Future[Int], char: Future[Char]): Future[(Int, Char)] =
    number.flatMap(n => char.map(c => (n, c)))

  // generalized API
  def getPairs[M[_] : Monad, A, B](ma: M[A], mb: M[B]): M[(A, B)] =
    Monad[M].flatMap(ma)(a => Monad[M].map(mb)(b => (a, b)))

  // extension methods - weirder imports - pure, flatMap

  import cats.syntax.applicative.*

  val oneOption = 1.pure[Option] // implicit Monad[Option] will be used => Some(1)
  val oneList = 1.pure[List] // List(1)

  import cats.syntax.flatMap.* // flatMap is here

  val oneOptionTransformed = oneOption.flatMap(x => (x + 1).pure[Option])

  // Monads extend Functors

  import cats.syntax.functor.* // map is here

  val optionMapped = Monad[Option].map(Option(2))(_ + 1) // map is here
  val optionmapped2 = oneOption.map(_ + 2)

  // for-comprehensions
  val composedOptionFor = for {
    one <- 1.pure[Option]
    two <- 2.pure[Option]
  } yield one + two

  // TODO 4: Implement a shorter version of getPairs using for-comprehensions
  def getPairs_v2[M[_] : Monad, A, B](ma: M[A], mb: M[B]): M[(A, B)] =
    for {
      a <- ma
      b <- mb
    } yield (a, b)

  def main(args: Array[String]): Unit = {
    println(getPairs_v2(numbersList, charsList))
    println(getPairs_v2(numberOption, charOption))
    getPairs_v2(numberFuture, charFuture).foreach(println)
  }

}
