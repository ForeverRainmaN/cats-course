package part1intro

object TypeClasses {

  case class Person(name: String, age: Int)

  // part1 - type class definition
  trait JSONSerializer[T] {
    def toJson(value: T): String
  }

  // part2 - implicit type class instances
  implicit object StringSerializer extends JSONSerializer[String] {
    override def toJson(value: String): String = "\"" + value + "\""
  }

  implicit object IntSerializer extends JSONSerializer[Int] {
    override def toJson(value: Int): String = value.toString
  }

  implicit object PersonSerializer extends JSONSerializer[Person] {
    override def toJson(value: Person): String =
      s"""
         |{ "name": ${value.name}, "age" : ${value.age}
         |""".stripMargin.trim
  }

  // part3 - offer some api
  def convertListToJson[T](list: List[T])(implicit serializer: JSONSerializer[T]): String =
    list.map(v => serializer.toJson(v)).mkString("[", ",", "]")

  def main(args: Array[String]): Unit = {
      println(convertListToJson(List(Person("Alice", 23), Person("Xavier", 45))))
  }

  // part 4 - extending the existing types via extension methods
  object JSONSyntax {
    implicit class JSONSerializable[T](value: T)(implicit serializer: JSONSerializer[T]) {
      def toJson: String = serializer.toJson(value)
    }
  }
}
