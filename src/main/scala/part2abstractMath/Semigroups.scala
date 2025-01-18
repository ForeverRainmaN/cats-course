package part2abstractMath

object Semigroups {

  // Semigroups combine elements of the same type

  import cats.Semigroup
  import cats.instances.int.*

  val naturalIntSemigroup = Semigroup[Int]
  val intCombination = naturalIntSemigroup.combine(2, 46) // addition

  import cats.instances.string.*

  val naturalStringSemigroup = Semigroup[String]
  val stringCombination = naturalStringSemigroup.combine("I love ", "Cats") // concatenations

  // specific api
  def reduceInts(list: List[Int]): Int =
    list.reduce(naturalIntSemigroup.combine)

  def reduceStrings(list: List[String]): String =
    list.reduce(naturalStringSemigroup.combine)

  // general api
  def reduceThings[T](list: List[T])(implicit semigroup: Semigroup[T]): T =
    list.reduce(semigroup.combine)

  // TODO 1: support a new type
  // hint: use the same pattern we used with Eq
  case class Expense(id: Long, amount: Double)

  implicit val expenseSemigroup: Semigroup[Expense] = Semigroup.instance[Expense] {
    (exp1, exp2) => Expense(Math.max(exp1.id, exp2.id), exp1.amount + exp2.amount)
  }

  // extension methods from Semigroup - |+|

  import cats.syntax.semigroup.*

  val anIntSum = 2 |+| 3 // requires the presence of an implicit Semigroup[Int]
  val aStringConcat = "we like " |+| "semigroups"
  val aCombinedExpense = Expense(12, 80) |+| Expense(25, 28.122)

  // TODO 2: implement reduceThings2 with the combination method (|+|)
  def reduceThings2[T: Semigroup](list: List[T]): T = list.reduce(_ |+| _)

  def main(args: Array[String]): Unit = {
    println(intCombination)
    println(stringCombination)

    // specific api
    val numbers = (1 to 10).toList
    val strings = List("I'm ", "starting ", "to ", "like ", "semigroups")
    println(reduceInts(numbers))
    println(reduceStrings(strings))

    // general API
    println(reduceThings(numbers)) // compiler injects the implicit Semigroup[Int]
    println(reduceThings(strings)) // compiler injects the implicit Semigroup[String]

    import cats.instances.option.*
    // compiler will produce an implicit Semigroup[Option[Int]] - combine will produce another option with the summed elements
    // compiler will produce an implicit Semigroup[Option[String]] - combine will produce another option with the concatenated elements
    val numberOptions: List[Option[Int]] = numbers.map(n => Option(n))
    println(reduceThings(numberOptions)) // an Option[Int] containing the sum of all the numbers
    val stringOptions: List[Option[String]] = strings.map(s => Option(s))
    println(reduceThings(stringOptions))

    // test exercise 1:
    val expensesList: List[Expense] = List(Expense(1, 25.0), Expense(2, 44.23), Expense(3, 49.18), Expense(25, 102.23))
    println(reduceThings(expensesList))
    // test exercise 2:
    println(reduceThings2(expensesList))
  }
}
