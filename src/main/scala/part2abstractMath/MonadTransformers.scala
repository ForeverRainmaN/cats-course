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
  val futureOfEither: EitherT[Future, String, Int] = EitherT.right(Future(45))

  val bandwidths = Map(
    "server1.rockthejvm.com" -> 50,
    "server2.rockthejvm.com" -> 300,
    "server3.rockthejvm.com" -> 170
  )

  type AsyncResponse[T] = EitherT[Future, String, T]

  def getBandwidth(server: String): AsyncResponse[Int] = bandwidths.get(server) match {
    case None => EitherT.left(Future(s"Server $server unreachable"))
    case Some(v) => EitherT.right(Future(v))
  }

  def canWithstandSurge(s1: String, s2: String): AsyncResponse[Boolean] = for {
    first <- getBandwidth(s1)
    second <- getBandwidth(s2)
  } yield first + second > 250
  // Future[Either[String, Boolean]]

  def generateTrafficSpikeReport(s1: String, s2: String): AsyncResponse[String] = {
    canWithstandSurge(s1, s2).transform[String, String] {
      case Left(reason) => Left(s"Servers $s1 and $s2 CANNOT cope with the incoming spike: not enough total bandwidth: $reason")
      case Right(false) => Left(s"Servers $s1 and $s2 CANNOT cope with the incoming spike: not enough total bandwidth")
      case Right(true) => Right(s"Servers $s1 and $s2 can cope with the incoming spike no problem!")
    }
  }

  def main(args: Array[String]): Unit = {
    val futureResult: Future[Either[String, String]] =
      generateTrafficSpikeReport("server2.rockthejvm.com", "server3.rockthejvm.com").value

    futureResult.foreach(println)
  }
}
