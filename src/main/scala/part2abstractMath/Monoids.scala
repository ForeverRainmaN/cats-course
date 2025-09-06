package part2abstractMath

object Monoids {

  import cats.instances.int.*
  import cats.syntax.semigroup.*

  val numbers = (1 to 1000).toList
  // |+| is always associative
  val sumLeft = numbers.foldLeft(0)(_ |+| _)
  val sumRight = numbers.foldRight(0)(_ |+| _)

  // define general api
  //  def combineFold[T](list: List[T])(implicit semigroup: Semigroup[T]): T =
  //    list.foldLeft(/* WHAT?! */)(_ |+| _)
  // MONOID

  import cats.Monoid

  val intMonoid = Monoid[Int]
  val combineInt = intMonoid.combine(23, 999) // 1022
  val zero = intMonoid.empty // 0

  import cats.instances.string.* // bring the implicit Monoid[String] into scope

  val emptyString = Monoid[String].empty
  val combineString = Monoid[String].combine("I understand ", "Monoids")

  import cats.instances.option.* // construct an implicit Monoid[Option[String]]

  val emptyOption = Monoid[Option[Int]].empty // None
  val combineOption = Monoid[Option[Int]].combine(Option(2), Option.empty[Int])
  val combineOption2 = Monoid[Option[Int]].combine(Option(3), Option(6))

  // extension methods for Monoids - |+|

  val combinedOptionFancy = Option(3) |+| Option(7)
  // implement a combineFold

  def combineFold[T: Monoid](list: List[T]) = list.foldLeft(Monoid.empty)(_ |+| _)

  val phoneBooks = List(
    Map(
      "Alice" -> 245,
      "Bob" -> 647
    ),
    Map(
      "Charlie" -> 372,
      "Daniel" -> 889
    ),
    Map(
      "Tina" -> 123
    )
  )

  case class ShoppingCart(items: List[String], total: Double)

  implicit val shoppingCartMonoid: Monoid[ShoppingCart] =
    Monoid.instance[ShoppingCart](
      ShoppingCart(List.empty, 0.0),
      (first, second) =>
        ShoppingCart(first.items ++ second.items, first.total + second.total))

  def checkout(shoppingCarts: List[ShoppingCart]): ShoppingCart = combineFold(shoppingCarts)

  def main(args: Array[String]): Unit = {
    println(sumLeft)
    println(sumRight)
    println(combineFold(numbers))
    println(combineFold(phoneBooks))
  }
}
