package part1intro

import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success, Try}

object Essentials {
  // values
  val aBoolean: Boolean = false
  // expressions are evaluated
  val anIfExpression = if (2 > 3) "bigger" else "smaller"

  // instructions vs expressions
  val theUnit = println("Hello, Scala")

  // OOP
  class Animal

  class Cat extends Animal

  trait Carnivore {
    def eat(animal: Animal): Unit
  }

  // inheritance model: extends <= 1 class but inherit from >= 0 traits

  class Crocodile extends Animal with Carnivore {
    override def eat(animal: Animal): Unit = println("Crunch!")
  }

  // Singleton
  object MySingleton // singleton pattern in one line

  // companions
  object Carnivore

  // generics
  class MyList[A]

  // method notation
  val three = 1 + 2
  val anotherThree = 1 + 2

  // functional programming
  val incrementer: Int => Int = x => x + 1
  val incremented = incrementer(45) // 46

  // map, flatMap, filter
  val processedList = List(1, 2, 3).map(incrementer) // List(2,3,4)
  val aLongerList = List(1, 2, 3).flatMap(x => List(x, x + 1)) // List(1,2,2,3,3,4)

  val anOption: Option[Int] = Option(3)
  val doubledOption: Option[Int] = anOption.map(_ * 3)

  val anAttempt = Try(/* something that might throw */ 42) // Success(42)
  val aModifiedAttempt = anAttempt.map(_ + 10)

  // pattern matching
  val anUnknown: Any = 45

  val ordinal = anUnknown match {
    case 1 => "first"
    case 2 => "second"
    case _ => "unknown"
  }

  val optionDescription = anOption match
    case Some(v) => s"the option is not empty: $v"
    case None => "the option is empty"

  // futures
  implicit val ec: ExecutionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(8))
  val aFuture = Future {
    42
  }
  // wait for completion (async)
  aFuture.onComplete {
    case Failure(exception) => println(s"The meaning of value failed: $exception")
    case Success(value) => println(s"The async meaning of life is $value")
  }

  // for comprehensions
  val checkerBoard = List(1, 2, 3).flatMap(n => List("a", "b", "c"))
  val anotherCheckerboard = for {
    n <- List(1, 2, 3)
    c <- List("a", "b", "c")
  } yield (n, c)

  // higher kinded types
  trait HigherKindedType[F[_]]

  trait SequenceChecker[F[_]] {
    def isSequential: Boolean
  }

  val listChecker = new SequenceChecker[List] {
    override def isSequential: Boolean = true
  }

  def main(args: Array[String]): Unit = {
    println(checkerBoard)
  }
}
