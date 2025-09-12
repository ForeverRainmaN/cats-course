package part2abstractMath

import scala.util.{Failure, Success, Try}

object UsingMonads {

  import cats.Monad
  import cats.instances.list.*

  val monadList = Monad[List] // fetch the implicit Monad[List]

  val aSimpleList = monadList.pure(2)
  val anExtendedList = monadList.flatMap(aSimpleList)(x => List(x, x + 1))
  // either is also a monad
  val aManualEither: Either[String, Int] = Right(42)

  type LoadingOr[T] = Either[String, T]
  type ErrorOr[T] = Either[Throwable, T]

  import cats.instances.either.*

  val loadingMonad = Monad[LoadingOr]
  val anEither = loadingMonad.pure(45) // LoadingOr[Int] == Right(45)
  val aChangedLoading = loadingMonad.flatMap(anEither)(x => if (x % 2 == 0) Right(x + 1) else Left("Loading meaning of life..."))

  case class OrderStatus(orderId: Long, status: String)

  def getOrderStatus(orderId: Long): LoadingOr[OrderStatus] = Right(OrderStatus(orderId, status = "Ready to ship"))

  def trackLocation(orderStatus: OrderStatus): LoadingOr[String] =
    if (orderStatus.orderId > 1000) Left("Not available yet, refreshing data...")
    else Right("Amsterdam, NL")

  val orderId = 457L
  val orderLocation = loadingMonad.flatMap(getOrderStatus(orderId))(trackLocation)

  import cats.syntax.flatMap.*
  import cats.syntax.functor.*

  val orderLocationBetter = getOrderStatus(orderId).flatMap(orderStatus => trackLocation(orderStatus))

  val orderLocationFor = for {
    orderStatus <- getOrderStatus(orderId)
    location <- trackLocation(orderStatus)
  } yield location

  case class Connection(host: String, port: String)

  val config = Map(
    "host" -> "localhost",
    "port" -> "4040"
  )

  trait HttpService[M[_]] {
    def getConnection(cfg: Map[String, String]): M[Connection]

    def issueRequest(connection: Connection, payload: String): M[String]
  }

  object HttpServiceOption extends HttpService[Option] {

    override def getConnection(cfg: Map[String, String]): Option[Connection] =
      for {
        h <- cfg.get("host")
        p <- cfg.get("port")
      } yield Connection(h, p)

    override def issueRequest(connection: Connection, payload: String): Option[String] =
      if (payload.length >= 20) None else Some(s"request $payload has been accepted")
  }

  object HttpServiceErrorOr extends HttpService[ErrorOr] {
    override def getConnection(cfg: Map[String, String]): ErrorOr[Connection] = {
      if (!cfg.contains("host") || !cfg.contains("port")) {
        Left(new RuntimeException("Connection could not be established: invalid configuration"))
      } else {
        Right(Connection(cfg("host"), cfg("port")))
      }
    }

    override def issueRequest(connection: Connection, payload: String): ErrorOr[String] =
      if (payload.length >= 20) Left(new RuntimeException("Payload is too large"))
      else Right(s"request $payload has been accepted")
  }

  def getResponse[M[_] : Monad](service: HttpService[M], payload: String): M[String] =
    for {
      conn <- service.getConnection(config)
      response <- service.issueRequest(conn, payload)
    } yield response
}
