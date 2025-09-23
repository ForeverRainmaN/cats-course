package part4typeclasses

import cats.Eval
import cats.kernel.Monoid
import cats.syntax.compose.*

object Folding {
  object ListExercises {
    def map[A, B](list: List[A])(f: A => B): List[B] = list.foldRight(List.empty[B]) {
      (a, acc) => f(a) :: acc
    }

    def flatMap[A, B](list: List[A])(f: A => List[B]): List[B] = list.foldLeft(List.empty[B]) {
      (acc, a) => acc.foldRight(f(a))(_ :: _)
    }

    def filter[A](list: List[A])(predicate: A => Boolean): List[A] = list.foldRight(List.empty[A]) {
      (a, acc) => if predicate(a) then a :: acc else acc
    }

    def combineAll[A](list: List[A])(implicit monoid: Monoid[A]): A =
      list.foldLeft(monoid.empty)(monoid.combine)
  }

  import cats.Foldable
  import cats.instances.list.* // implicit Foldable[List]

  val sum = Foldable[List].foldLeft(List(1, 2, 3), 0)(_ + _) // 6

  import cats.instances.option.* // implicit Foldable[Option]

  val sumOption = Foldable[Option].foldLeft(Option(2), 30)(_ + _) // 32

  // foldRight is stack-safe regardless of your container
  val sumRight = Foldable[List].foldRight(List(1, 2, 3), Eval.now(0)) { (num, eval) =>
    eval.map(_ + num)
  }

  import cats.instances.int.*

  val anotherSum = Foldable[List].combineAll(List(1, 2, 3)) // implicit Monoid[Int]
  import cats.instances.string.*

  val mappedConcat = Foldable[List].foldMap(List(1, 2, 3))(_.toString)

  import cats.instances.vector.*

  val intsNested = List(Vector(1, 2, 3), Vector(4, 5, 6))
//  (Foldable[List] compose Foldable[Vector]).combineAll(intsNested)

  import cats.syntax.foldable.*

  val sum3 = List(1, 2, 3).combineAll
  val mappedConcat2 = List(1, 2, 3).foldMap(_.toString)

  def main(args: Array[String]): Unit = {
    val list = List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
    println(ListExercises.map(list)(_ + 1))
    println(ListExercises.flatMap(list)(x => List(x, x + 1)))
    println(ListExercises.filter(list)(_ > 5))
    println(ListExercises.combineAll(list))
  }
}
