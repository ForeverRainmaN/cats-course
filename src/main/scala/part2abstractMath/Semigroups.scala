package part2abstractMath

object Semigroups {
  // Semigroups COMBINE elements of the same type

  import cats.Semigroup
  import cats.instances.int.*

  val naturalIntSemigroup = Semigroup[Int]
  val intCombination = naturalIntSemigroup.combine(2, 46) // addition

  import cats.instances.string.*

  val naturalStringSemigroup = Semigroup[String]
  val stringCombination = naturalStringSemigroup.combine("I love", "Cats") // concatenation

  // specific api
  def reduceInts(list: List[Int]): Int = list.reduce(naturalIntSemigroup.combine)

  def reduceStrings(list: List[String]): String = list.reduce(naturalStringSemigroup.combine)

  // general API
  def reduceThings[T](list: List[T])(implicit semigroup: Semigroup[T]): T = list.reduce(semigroup.combine)

  case class Expense(id: Long, amount: Double)

  implicit val expenseSemigroup: Semigroup[Expense] =
    Semigroup.instance[Expense] { (ex1, ex2) => Expense(Math.max(ex1.id, ex2.id), ex1.amount + ex2.amount) }

  //  implicit object ExpenseSemigroup extends Semigroup[Expense] {
  //
  //    override def combine(x: Expense, y: Expense): Expense = Expense(x.id + y.id, x.amount + y.amount)
  //  }

  // extension methods from Semigroup - |+|

  import cats.syntax.semigroup.*

  val anIntSum = 2 |+| 3
  val aStringConcat = "we like " |+| "semigroups"
  val aCombinedExpense = Expense(4, 80) |+| Expense(56, 46)

  def reduceThings2[T: Semigroup](list: List[T]): T = list.reduce(_ |+| _)

  def main(args: Array[String]): Unit = {
    println(intCombination)
    println(stringCombination)
    val numbers = (1 to 10).toList

    val strings = List("I'm ", "starting ", "to ", "like ", "semigroups")
    println(reduceInts(numbers))
    println(reduceStrings(strings))

    import cats.instances.option.*
    val numberOptions: List[Option[Int]] = numbers.map(Option(_))
    println(reduceThings(numberOptions))

    val stringOptions: List[Option[String]] = strings.map(Option(_))

    val expenseList = List(Expense(25L, 15L), Expense(50L, 45L))
    println(reduceThings(expenseList))
    println(reduceThings(stringOptions))
  }
}
