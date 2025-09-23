package part4typeclasses

import cats.{Applicative, Foldable, Functor}

import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, Future}

object Traversing {

  implicit val ec: ExecutionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(8))
  val servers: List[String] = List("server-ci.rockthejvm.com", "server-staging.rockthejvm.com", "prod.rockthejvm.com")

  def getBandwidth(hostname: String): Future[Int] = Future(hostname.length * 80)

  /*
    we have a List[String]
    a func String => Future[Int]
    we want a Future[List[Int]]
   */

  val allBandwidths: Future[List[Int]] = servers.foldLeft(Future(List.empty[Int])) { (accumulator, hostname) =>
    val bandFuture: Future[Int] = getBandwidth(hostname)
    for {
      accBandwidths <- accumulator
      band <- bandFuture
    } yield accBandwidths :+ band
  }

  val allBandwidthsTraverse: Future[List[Int]] = Future.traverse(servers)(getBandwidth)
  val allBandwidthSequence: Future[List[Int]] = Future.sequence(servers.map(getBandwidth))

  import cats.syntax.applicative.*
  import cats.syntax.apply.* // mapN

  def listTraverse[F[_] : Applicative, A, B](list: List[A])(f: A => F[B]): F[List[B]] = {
    list.foldLeft(List.empty[B].pure[F]) { (acc, value) =>
      val element: F[B] = f(value)
      (acc, element).mapN(_ :+ _)
    }
  }

  def listSequence[F[_] : Applicative, A](list: List[F[A]]): F[List[A]] =
    list.foldLeft(List.empty[A].pure[F]) { (acc, value) =>
      (acc, value).mapN(_ :+ _)
    }

  def listSequence2[F[_] : Applicative, A](list: List[F[A]]): F[List[A]] =
    listTraverse(list)(identity)

  // List[Vector[Int]] -> Vector[List[Int]]
  val allPairs = listSequence(List(Vector(1, 2), Vector(3, 4))) // Vector(List(1, 3), List(1, 4), List(2, 3), List(2, 4))
  val threePairs = listSequence(List(Vector(1, 2), Vector(3, 4), Vector(5, 6))) // Vector(List(1, 3, 5), List(1, 3, 6), List(1, 4, 5), List(1, 4, 6), List(2, 3, 5), List(2, 3, 6), List(2, 4, 5), List(2, 4, 6))

  def filterAsOption(list: List[Int])(predicate: Int => Boolean): Option[List[Int]] =
    listTraverse[Option, Int, Int](list)(n => Some(n).filter(predicate))

  val allTrue = filterAsOption(List(2, 4, 6))(x => x % 2 == 0) // Some(List(2,4,6))
  val someFalse = filterAsOption(List(1, 2, 3))(_ % 2 == 0) // None

  import cats.data.Validated

  type ErrorsOr[T] = Validated[List[String], T]

  def filterAsValidated(list: List[Int])(predicate: Int => Boolean): ErrorsOr[List[Int]] =
    listTraverse[ErrorsOr, Int, Int](list) { n =>
      if (predicate(n)) Validated.valid(n)
      else Validated.invalid(List(s"predicate for $n failed"))
    }

  val allTrueValidated = filterAsValidated(List(2, 4, 6))(_ % 2 == 0) // Valid(List(2,4,6))
  val someFalseValidated = filterAsValidated(List(1, 2, 3))(_ % 2 == 0) // Invalid(List("predicate for 1 failed", "predicate for 3 failed"))

  trait MyTraverse[L[_]] extends Foldable[L], Functor[L] {
    def traverse[F[_] : Applicative, A, B](container: L[A])(f: A => F[B]): F[L[B]]

    def sequence[F[_] : Applicative, A](container: L[F[A]]): F[L[A]] =
      traverse(container)(identity)

    import cats.Id

    def map[A, B](wa: L[A])(f: A => B): L[B] = {
      traverse[Id, A, B](wa)(f)
    }
  }

  import cats.Traverse

  val allBandwidthsCats = Traverse[List].traverse(servers)(getBandwidth)

  import cats.syntax.traverse.* // sequence + traverse

  val allBandwidthsCats2 = servers.traverse(getBandwidth)


  def main(args: Array[String]): Unit = {

  }
}
