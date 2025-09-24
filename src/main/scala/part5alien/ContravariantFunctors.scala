package part5alien

object ContravariantFunctors {

  trait Format[T] { // contravariant type classes
    self =>
    def format(value: T): String

    def contramap[A](func: A => T): Format[A] = new Format[A] {
      override def format(value: A): String = self.format(func(value))
    }
  }

  def format[A](value: A)(implicit f: Format[A]) = f.format(value)

  implicit object StringFormat extends Format[String] {
    override def format(value: String): String = "\"" + value + "\""
  }

  implicit object IntFormat extends Format[Int] {
    override def format(value: Int): String = value.toString
  }

  implicit object BooleanFormat extends Format[Boolean] {
    override def format(value: Boolean): String = if (value) "Y" else "N"
  }

  // problem: given Format[MyType], can we have a Format[Option[MyType]]?
  implicit def getOptionFormat[T](implicit f: Format[T]): Format[Option[T]] = f.contramap[Option[T]](_.get)
  /*
   IntFormat
   fo: Format[Option[Int]] = IntFormat.contramap[Option[Int]](_.get)
   fo2: Format[Option[Option[Int]]] = fo.contramap[Option[Option[Int]]](_.get) // second get

   fo2 = IntFormat
        .contramap
        .contramap

  Map applies transformations in sequence
  Contramap applies transformations in REVERSE sequence
   */

  import cats.instances.int.*
  import cats.{Contravariant, Show}

  val showInts = Show[Int]
  val showOption: Show[Option[Int]] = Contravariant[Show].contramap(showInts)(_.getOrElse(0))

  import cats.syntax.contravariant.*

  val showOptionsShorter: Show[Option[Int]] = showInts.contramap(_.getOrElse(0))

  def main(args: Array[String]): Unit = {

  }
}
