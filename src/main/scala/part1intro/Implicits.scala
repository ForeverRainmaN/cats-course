package part1intro

object Implicits {

  // implicit classes
  case class Person(name: String) {
    def greet: String = s"Hi, my name is $name!"
  }

  implicit class ImpersonableString(name: String) {
    def greet: String = Person(name).greet
  }

  val impersonableString = new ImpersonableString("Peter")

  val greeting = "Peeter".greet // new ImpersonableString("Peter").greet

  // importing implicit conversions in scope

  import scala.concurrent.duration.*

  val oneSec = 1.second

  // implicit arguments and values
  def increment(x: Int)(implicit amount: Int) = x + amount

  val incremented2 = increment(2)
  
  implicit val defaultAmount: Int = 10

  def multiply(x: Int)(implicit times: Int) = x * times

  val times2 = multiply(2)

  // more complex example
  trait JSONSerializer[T] {
    def toJSON(value: T): String
  }

  def listToJson[T](list: List[T])(implicit serializer: JSONSerializer[T]): String =
    list.map(value => serializer.toJSON(value)).mkString("[", ",", "]")

  val personJson = listToJson(List(Person("Alice"), Person("Bob")))

  // implicit methods
  implicit def oneArgCaseClassSerializer[T <: Product]: JSONSerializer[T] = (value: T) =>
    s"""
       | "${value.productElementName(0)}" : "${value.productElement(0)}"
       |""".stripMargin.trim

  def main(args: Array[String]): Unit = {

  }
}
