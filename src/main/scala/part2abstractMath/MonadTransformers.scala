package part2abstractMath

import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, Future}

object MonadTransformers {
  def sumAllOptions(values: List[Option[Int]]): Int = ???

  // option transformer

  import cats.data.OptionT
  import cats.instances.list.*

  val listOfNumberOptions: OptionT[List, Int] = OptionT(List(Option(1), Option(2)))
  val listOfCharOptions: OptionT[List, Char] = OptionT(List(Option('a'), Option('b'), Option.empty[Char]))

  val listOfTuples: OptionT[List, (Int, Char)] = for {
    char <- listOfCharOptions
    number <- listOfNumberOptions
  } yield (number, char)

  // either transformer

  import cats.data.EitherT

  val listOfEithers: EitherT[List, String, Int] = EitherT(List(Left("something wrong"), Right(43), Right(2)))
  implicit val ec: ExecutionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(8))
  val futureOfEither: EitherT[Future, String, Int] = EitherT(Future(Right(45)))

  val bandwidths = Map(
    "server1.rockthejvm.com" -> 50,
    "server2.rockthejvm.com" -> 300,
    "server3.rockthejvm.com" -> 170
  )

  type AsyncResponse[T] = EitherT[Future, String, T]

  def getBandwidth(server: String): AsyncResponse[Int] = bandwidths.get(server) match {
    case None => EitherT(Future(Left("Server unreachable")))
    case Some(v) => EitherT(Future(Right(v)))
  }

  def canWithstandSurge(s1: String, s2: String): AsyncResponse[Boolean] = {
    val bandwidthSum = for {
      first <- getBandwidth(s1)
      second <- getBandwidth(s2)
    } yield first + second

    bandwidthSum.flatMap(x => if x > 250 then EitherT[Future, String, Boolean](Future(Right(true))) else EitherT(Future(Left("Unavailable"))))
  }

  def generateTrafficSpikeReport(s1: String, s2: String): AsyncResponse[String] = {
    canWithstandSurge(s1, s2).transform[String, String] {
      case Left(value) => Left("huynya")
      case Right(value) => Right("Huynya2")
    }
  }

  def main(args: Array[String]): Unit = {
    println(listOfTuples.value)
  }
}
