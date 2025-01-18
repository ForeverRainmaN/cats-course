package part2abstractMath

object Monoids {

  import cats.instances.int.*
  import cats.syntax.semigroup.* // import the |+| extension method

  val numbers = (1 to 1000).toList
  // |+| is always associative
  val sumLeft = numbers.foldLeft(0)(_ |+| _)
  val sumRight = numbers.foldRight(0)(_ |+| _)

  // define a general API
  //    list.foldLeft()(_ |+| _)

  // MONOID

  import cats.Monoid

  val intMonoid = Monoid[Int]
  val combineInt = intMonoid.combine(23, 999) // 1024
  val zero = intMonoid.empty // 0

  import cats.instances.string.* // bring the implicit Monoid[String] in scope

  val emptyString = Monoid[String].empty
  val combineString = Monoid[String].combine("I understand ", "monoids")

  import cats.instances.option.* // construct an implicit Monoid[Option[Int]]

  val emptyOption = Monoid[Option[Int]].empty
  val combineOption = Monoid[Option[Int]].combine(Option(2), Option.empty[Int]) // Some(2)
  val combineOption2 = Monoid[Option[Int]].combine(Option(3), Option(6)) // Some(9)

  // extension method for Monoids - |+|
  // import cats.syntax.monoid.* either this one or cats.syntax.semigroup._
  val combinedOptionFancy = Option(3) |+| Option(7)

  // TODO 1: implement a combineFold
  def combineFold[T](list: List[T])(implicit monoid: Monoid[T]): T =
    list.foldLeft(monoid.empty)(_ |+| _)


  // TODO 2: Combine a list of phonebook as Maps[String, Int]
  val phoneBooks = List(
    Map(
      "Alice" -> 235,
      "Bob" -> 647,
    ),
    Map(
      "Charlie" -> 372,
      "Daniel" -> 889
    ),
    Map(
      "Tina" -> 123
    )
  )

  // TODO 3 - shopping cart and online stores with Monoids
  case class ShoppingCart(items: List[String], total: Double)

  implicit val shoppingCartMonoid: Monoid[ShoppingCart] = Monoid.instance(
    ShoppingCart(List.empty, 0.0), (sc1, sc2) => ShoppingCart(sc1.items ++ sc2.items, sc1.total + sc2.total)
  )

  def checkout(shoppingCarts: List[ShoppingCart]): ShoppingCart = combineFold(shoppingCarts)

  // constructing monoid yourself
  //  implicit val phoneBookMonoid: Monoid[Map[String, Int]] = Monoid.instance(
  //    Map[String, Int]().empty, (pb1, pb2) => pb1 ++ pb2
  //  )

  import cats.instances.map.*

  def main(args: Array[String]): Unit = {
    println(sumLeft)
    println(sumRight)
    val sc1 = ShoppingCart(List("Car", "Phone", "Flowers"), 325.22)
    val sc2 = ShoppingCart(List("Apples", "Book", "Table"), 15.22)
    val sc3 = ShoppingCart(List("Pen", "Notebook"), 200)
    val sc4 = ShoppingCart(List("Chair", "Toy", "Water", "Fridge"), 9000)

    val scList = List(sc1, sc2, sc3, sc4)
    println(combineFold(scList))
  }
}
