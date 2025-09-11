package part2abstractMath

object Readers {
  /*
    - configuration file => initial data structure
    - a DB layer
    - an HTTP layer
    - a business logic layer
   */

  case class Configuration(dbUsername: String, dbPassword: String, host: String, port: Int, nThreads: Int, emailReplyTo: String)

  case class DbConnection(username: String, password: String) {
    def getOrderStatus(orderId: Long): String = "dispatched"

    def getLastOrderId(username: String): Long = 542643
  }

  case class HttpService(host: String, port: Int) {
    def start(): Unit = println("Server started")
  }

  val config = Configuration("daniel", "rockthejvm1!", "localhost", 1234, 8, "daniel@rockthejvm.com")
  // cats Reader

  import cats.data.Reader

  val dbReader: Reader[Configuration, DbConnection] = Reader(config => DbConnection(config.dbUsername, config.dbPassword))
  val dbConn = dbReader.run(config)

  val danielsOrderStatusReader: Reader[Configuration, String] = dbReader.map(dbConn => dbConn.getOrderStatus(55))
  val danielsOrderStatus: String = danielsOrderStatusReader.run(config)

  def getLastOrderStatus(username: String): String = {
    //    val usersLastOrderStatus: Reader[Configuration, String] = dbReader
    //      .map(_.getLastOrderId(username))
    //      .flatMap(lastOrderId => dbReader.map(_.getOrderStatus(lastOrderId)))
    //    usersLastOrderStatus.run(config)

    val usersOrderFor = for {
      lastOrderId <- dbReader.map(_.getLastOrderId(username))
      orderStatus <- dbReader.map(_.getOrderStatus(lastOrderId))
    } yield orderStatus

    usersOrderFor.run(config)
  }

  /*
    Pattern:
  1. You create the initial data structure
  2. You create a reader which specifies how that data structure will be manipulated later
  3. You can then map & flatMap the reader to produce derived information
  4. when you need the final piece of information, you call run on the reader with the initial data structure
  */

  case class EmailService(emailReplyTo: String) {
    def sendEmail(address: String, contents: String) = s"from $emailReplyTo; to: $address >>> $contents"
  }

  def emailUser(username: String, userEmail: String) = {
    val emailServiceReader: Reader[Configuration, EmailService] = Reader(config => EmailService(config.emailReplyTo))
    val emailReader = for {
      lastOrderId <- dbReader.map(_.getLastOrderId(username))
      orderStatus <- dbReader.map(_.getOrderStatus(lastOrderId))
      emailService <- emailServiceReader
    } yield emailService.sendEmail(userEmail, s"your last order has the status: ($orderStatus)")

    emailReader.run(config)
  }

  def main(args: Array[String]): Unit = {
    println(getLastOrderStatus("daniel"))
    println(emailUser("daniel", "daniel@rtjvm.com"))
  }
}
