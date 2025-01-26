package part2abstractMath

import scala.util.{Failure, Success, Try}

object UsingMonads {

  import cats.Monad
  import cats.instances.list.*

  val monadList = Monad[List] // fetch the implicit Monad[List]
  val aSimpleList = monadList.pure(2) // List(2)
  val anExtendedList = monadList.flatMap(aSimpleList)(x => List(x, x + 1))

  // applicable to Option, Try, Future

  val aManualEither: Either[String, Int] = Right(42)

  // either is also a monad
  type LoadingOr[T] = Either[String, T]
  type ErrorOr[T] = Either[Throwable, T]

  import cats.instances.either.*

  val loadingMonad = Monad[LoadingOr]

  val anEither = loadingMonad.pure(45) // LoadingOr[Int] == Right(45)

  val aChangedLoading = loadingMonad.flatMap(anEither)(n =>
    if (n % 2 == 0) Right(n + 1) else Left("Loading meaning of life...")
  )

  // imaginary online store
  case class OrderStatus(orderId: Long, status: String)

  def getOrderStatus(orderId: Long): LoadingOr[OrderStatus] =
    Right(OrderStatus(orderId, "Ready to ship"))

  def trackLocation(orderStatus: OrderStatus): LoadingOr[String] =
    if (orderStatus.orderId > 1000) Left("Not available yet, refreshing data...")
    else Right("Amsterdam, NL")

  val orderId = 457L
  val orderLocation = loadingMonad.flatMap(getOrderStatus(orderId))(trackLocation)
  // use extension methods

  import cats.syntax.functor.*

  val orderLocationBetter: LoadingOr[String] = getOrderStatus(orderId).flatMap(trackLocation)

  val orderLocationFor: LoadingOr[String] = for {
    orderStatus <- getOrderStatus(orderId)
    location <- trackLocation(orderStatus)
  } yield location

  // TODO: thee service layer API of a web app

  case class Connection(host: String, port: String)

  val config = Map(
    "host" -> "localhost",
    "port" -> "4040"
  )

  trait HttpService[M[_]] {
    def getConnection(cfg: Map[String, String]): M[Connection]

    def issueRequest(connection: Connection, payload: String): M[String]
  }

  import cats.syntax.flatMap.*
  import cats.syntax.functor.*

  // general API
  def getResponse[M[_]](service: HttpService[M], payload: String)(implicit monad: Monad[M]): M[String] =
    for {
      conn <- service.getConnection(config)
      response <- service.issueRequest(conn, payload)
    } yield response

  // DO NOT CHANGE THE CODE
  /*
  -  if the host and port are found in the configuration map, then we'll return a M containing a connection with those values
    otherwise the method will fail according to the logic of the type M
  - the issueRequest method returns a M containing the string: "request (payload) has been accepted", if the payload is less than 20 chars
    otherwise the method will fail, according to the logic of the type M

    provide a real implementation of HttpService using Try, Option, Future, Either
  */
  // futures

  // couldve used object TryHttpService extends HttpService[Try]
  val httpServiceTry: HttpService[Try] = new HttpService[Try] {
    override def issueRequest(connection: Connection, payload: String): Try[String] =
      payload match {
        case a if a.length >= 20 => Failure(IllegalStateException("payload is too long"))
        case _ => Success(s"payload $payload has been accepted")
      }

    override def getConnection(cfg: Map[String, String]): Try[Connection] =
      if (cfg.contains("host") && cfg.contains("port")) Success(Connection(cfg("host"), cfg("port")))
      else Failure(IllegalStateException("Port or Host is not present in configuration"))
  }

  // version with for-comprehension
  val httpServiceOption: HttpService[Option] = new HttpService[Option] {
    override def getConnection(cfg: Map[String, String]): Option[Connection] =
      for {
        h <- cfg.get("host")
        p <- cfg.get("port")
      } yield Connection(h, p)

    override def issueRequest(connection: Connection, payload: String): Option[String] =
      payload match {
        case a if a.length >= 20 => None
        case _ => Some(s"request $payload has been accepted")
      }
  }

  import java.util.concurrent.Executors
  import scala.concurrent.{ExecutionContext, Future}

  implicit val ec: ExecutionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(8))

  val httpServiceFuture: HttpService[Future] = new HttpService[Future] {
    override def issueRequest(connection: Connection, payload: String): Future[String] =
      payload match {
        case a if a.length >= 20 => Future.failed(IllegalStateException("payload is too long"))
        case _ => Future.successful(s"request $payload has been accepted")
      }

    override def getConnection(cfg: Map[String, String]): Future[Connection] =
      if (cfg.contains("host") && cfg.contains("port")) Future.successful(Connection(cfg("host"), cfg("port")))
      else Future.failed(new IllegalStateException("Port or Host is not present in configuration"))
  }

  val httpServiceEither: HttpService[ErrorOr] = new HttpService[ErrorOr] {
    override def getConnection(cfg: Map[String, String]): ErrorOr[Connection] =
      if (cfg.contains("host") && cfg.contains("port")) Right(Connection(cfg("host"), cfg("port")))
      else Left(RuntimeException("Port or Host is not present in configuration"))

    override def issueRequest(connection: Connection, payload: String): ErrorOr[String] =
      payload match {
        case a if a.length >= 20 => Left(RuntimeException("payload is too long"))
        case _ => Right(s"request $payload has been accepted")
      }
  }

  def main(args: Array[String]): Unit = {
    val errorOrResponse = for {
      conn <- httpServiceEither.getConnection(config)
      response <- httpServiceEither.issueRequest(conn, "Hello Http Service")
    } yield response

    println(errorOrResponse)
  }
}
