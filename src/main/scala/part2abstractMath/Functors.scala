package part2abstractMath

import scala.util.Try

object Functors {

  val aModifiedList = List(1, 2, 3).map(_ + 1) // List(2,3,4)
  val aModifiedOption = Option(2).map(_ + 1) // Some(2)
  val aModifiedTry = Try(42).map(_ + 1) // Success(43)

  // simplified definition
  trait MyFunctor[F[_]] {
    def map[A, B](initialValue: F[A])(f: A => B): F[B]
  }

  import cats.Functor
  import cats.instances.list.* // includes Functor[List]

  val listFunctor = Functor[List]
  val incrementedNumbers = listFunctor.map(List(1, 2, 3))(_ + 1)

  import cats.instances.option.* // includes functor option

  val optionFunctor = Functor[Option]
  val incrementedOption = optionFunctor.map(Option(2))(_ + 1) // Some(3)

  import cats.instances.try_.*

  val anIncrementedTry = Functor[Try].map(Try(42))(_ + 1) // Some(3)

  // generalizing an API

  def do10xList(list: List[Int]): List[Int] = list.map(_ * 10)

  def do10xOption(option: Option[Int]): Option[Int] = option.map(_ * 10)

  def do10xTry(attempt: Try[Int]): Try[Int] = attempt.map(_ * 10)

  def do10x[F[_]](container: F[Int])(implicit functor: Functor[F]): F[Int] = functor.map(container)(_ * 10)

  trait Tree[+T]

  object Tree {
    // smart constructors
    def leaf[T](value: T) = Leaf(value)

    def branch[T](value: T, left: Tree[T], right: Tree[T]) = Branch(value, left, right)
  }

  case class Leaf[+T](value: T) extends Tree[T]

  case class Branch[+T](value: T, left: Tree[T], right: Tree[T]) extends Tree[T]

  implicit object TreeFunctor extends Functor[Tree] {
    override def map[A, B](fa: Tree[A])(f: A => B): Tree[B] = fa match
      case Branch(v, left, right) => Branch(f(v), map(left)(f), map(right)(f))
      case Leaf(v) => Leaf(f(v))
  }

  // extension method - map

  import cats.syntax.functor.*

  val tree: Tree[Int] = Tree.branch(40, Tree.branch(5, Tree.leaf(10), Tree.leaf(30)), Tree.leaf(20))
  val incrementedTree = tree.map(_ + 1)

  def do10xShorter[F[_] : Functor](container: F[Int]): F[Int] = container.map(_ * 10)

  def main(args: Array[String]): Unit = {
    println(do10xShorter(List(1, 2, 3)))
    println(do10xShorter(Option(2)))
    println(do10xShorter(Try(35)))

    println(do10xShorter[Tree](Tree.branch(30, Tree.leaf(10), Tree.leaf(20))))
  }
}