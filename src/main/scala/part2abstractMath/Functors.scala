package part2abstractMath

import scala.util.Try

object Functors {

  val aModifiedList = List(1, 2, 3).map(_ + 1) // List(2,3,4)
  val aModifiedOption = Option(2).map(_ + 1) // Some(3)
  val aModifiedTry = Try(42).map(_ + 1) // Success(43)

  // simplified definition
  trait MyFunctor[F[_]] {
    def map[A, B](initialValue: F[A])(f: A => B): F[B]
  }

  import cats.Functor
  import cats.instances.list.*

  val listFunctor = Functor[List]
  val incrementedNumbers = listFunctor.map(List(1, 2, 3))(_ + 1) // List(2,3,4)

  import cats.instances.option.*

  val optionFunctor = Functor[Option]
  val incrementedOption = optionFunctor.map(Option(2))(_ + 1) // Some(3)

  import cats.instances.try_.*

  val anIncrementedTry = Functor[Try]().map(Try(42))(_ + 1) // Success(43)

  // generalizing an API
  def do10xList(list: List[Int]): List[Int] = list.map(_ * 10)

  def do10xOption(option: Option[Int]): Option[Int] = option.map(_ * 10)

  def do10xTry(attempt: Try[Int]): Try[Int] = attempt.map(_ * 10)

  // generalize
  def do10x[F[_] : Functor](container: F[Int]): F[Int] =
    Functor[F].map(container)(_ * 10)

  // TODO 1: define your own functor for a binary tree
  // hint: define an object which extends Functor[Tree]
  sealed trait Tree[+T]

  private object Tree {
    // "smart" constructors
    def leaf[T](value: T): Tree[T] = Leaf(value)

    def branch[T](v: T, left: Tree[T], right: Tree[T]): Tree[T] = Branch(v, left, right)
  }

  sealed case class Leaf[+T](value: T) extends Tree[T]

  sealed case class Branch[+T](value: T, left: Tree[T], right: Tree[T]) extends Tree[T]

  implicit object TreeFunctor extends Functor[Tree]:
    override def map[A, B](fa: Tree[A])(f: A => B): Tree[B] = fa match
      case Leaf(value) => Leaf(f(value))
      case Branch(value, left, right) =>
        Branch(f(value), map(left)(f), map(right)(f))

  // extension method - map

  import cats.syntax.functor.*

  val tree: Tree[Int] = Tree.branch(40, Tree.branch(5, Tree.leaf(10), Tree.leaf(30)), Tree.leaf(20))
  val incrementedTree = tree.map(_ * 10)

  // TODO 2: shorter version of do10x method
  def do10x2[F[_] : Functor](container: F[Int]): F[Int] =
    container.map(_ * 10)

  def main(args: Array[String]): Unit = {
    //    println(do10x(List(1, 2, 3)))
    //    println(do10x(Option(2)))
    //    println(do10x(Try(35)))
    val tree = Tree.branch(1, Tree.leaf(2), Tree.branch(3, Tree.leaf(4), Tree.leaf(5)))
    val res = do10x2(tree)
    println(res)
  }
}
