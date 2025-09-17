package part3datamanipulation

import cats.Semigroup

import scala.annotation.tailrec
import scala.util.Try

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

  aValidValue.andThen(_ => anInvalidValue)
  // test a valid value with ensure
  aValidValue.ensure(List("Something went wrong"))(_ % 2 == 0)
  // transform
  aValidValue.map(_ + 1)
  aValidValue.leftMap(_.length)
  aValidValue.bimap(_.length, _ + 1)
  // interoperate with stdlib
  val eitherToValidated: Validated[List[String], Int] = Validated.fromEither(Right(42))
  val optionToValidated: Validated[List[String], Int] = Validated.fromOption(None, List("Nothing present here"))
  val tryToValidated: Validated[Throwable, Int] = Validated.fromTry(Try("Something".toInt))
  // backwards
  aValidValue.toOption
  aValidValue.toEither

  object FormValidation {

    import cats.data.Validated

    type FormValidation[T] = Validated[List[String], T]

    def getValue(form: Map[String, String], fieldName: String): FormValidation[String] =
      Validated.fromOption(form.get(fieldName), List(s"the field $fieldName must be specified."))

    def nonBlank(value: String, fieldName: String): FormValidation[String] =
      Validated.cond(value.nonEmpty, value, List(s"The field $fieldName must not be blank"))

    def emailProperForm(email: String): FormValidation[String] =
      Validated.cond(email.contains("@"), email, List("Email is invalid"))

    def passwordCheck(password: String): FormValidation[String] =
      Validated.cond(password.length >= 10, password, List("Passwords must be at least 10 characters long."))

    def validateForm(form: Map[String, String]): FormValidation[String] = {
      getValue(form, "Name").andThen(name => nonBlank(name, "Name"))
        .combine(getValue(form, "Email").andThen(emailProperForm))
        .combine(getValue(form, "Password").andThen(passwordCheck))
        .map(_ => "User registration complete")
    }
  }

  import cats.syntax.validated.*

  val anInvalidMeaningOfLife: Validated[List[String], Int] = 42.valid[List[String]]
  val anError: Validated[String, Int] = "Something went wrong".invalid[Int]

  def main(args: Array[String]): Unit = {
    println(testNumber(10))
  }
}
