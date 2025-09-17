package part3datamanipulation

import cats.Semigroup

import scala.annotation.tailrec

object DataValidation {

  import cats.data.Validated

  val aValidValue: Validated[String, Int] = Validated.valid(42) // "right" value of Either
  val anInvalidValue: Validated[String, Int] = Validated.invalid("Something went wrong") // "left" value of Either
  val aTest: Validated[String, Int] = Validated.cond(42 > 39, 99, "Meaning of life is too small")

  //  def testNumber(n: Int): Either[List[String], Int] = {
  //    var validations: List[String] = List()
  //
  //    if (!isPrime(n)) validations = validations :+ "n must be prime"
  //    if (n < 0) validations = validations :+ "n must be non negative"
  //    if (n <= 100) validations = validations :+ "n <= 100"
  //    if (n % 2 != 0) validations = validations :+ "must be even"
  //
  //    if (validations.nonEmpty) Left(validations)
  //    else Right(n)
  //  }

  def testNumber(n: Int): Either[List[String], Int] = {
    val isNotEven: List[String] = if (n % 2 == 0) List() else List("Number is not even")
    val isNegative: List[String] = if (n >= 0) List() else List("Number must be non-negative")
    val isTooBig: List[String] = if (n <= 100) List() else List("Number must be less than or equal to 100")
    val isNotPrime: List[String] = if (isPrime(n)) List() else List("Number must be prime")

    if (n % 2 == 0 && n >= 0 && n <= 100 && isPrime(n)) Right(n)
    else Left(isNotEven ++ isNegative ++ isTooBig ++ isNotPrime)
  }

  import cats.instances.list.*

  implicit val combineIntMax: Semigroup[Int] = Semigroup.instance[Int](Math.max)

  def validateNumber(n: Int): Validated[List[String], Int] =
    Validated.cond(n % 2 == 0, n, List("Number must be even"))
      .combine(Validated.cond(n >= 0, n, List("Number must be non-negative")))
      .combine(Validated.cond(n <= 100, n, List("Number must be less than or equal to 100")))
      .combine(Validated.cond(isPrime(n), n, List("Number must be a prime")))

  def isPrime(n: Int): Boolean = {
    if (n <= 1) false
    else if (n == 2) true
    else if (n % 2 == 0) false
    else {
      val maxDivisor = Math.sqrt(n).toInt

      @tailrec
      def go(divisor: Int): Boolean = {
        if (divisor > maxDivisor) true
        else if (n % divisor == 0) false
        else go(divisor + 2)
      }

      go(3)
    }
  }

  def main(args: Array[String]): Unit = {
    println(testNumber(10))
  }
}
