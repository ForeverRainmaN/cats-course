package part3datamanipulation

object Evaluation {
  /*
    Cats makes the distinction between
      - evaluating an expression eagerly
      - evaluating lazily and every time you request it
      - evaluation lazily and keeping the value (memoizing)
   */

  import cats.Eval

  val instantEval: Eval[Int] = Eval.now {
    println("Computing now!")
    64345
  }

  val redoEval = Eval.always {
    println("Computing again!")
    4324
  }

  val delayedEval = Eval.later {
    println("Computing later!")
    53278
  }

  val composedEvaluation = instantEval.flatMap(value1 => delayedEval.map(value2 => value1 + value2))
  val anotherComposedEvaluation = for {
    value1 <- instantEval
    value2 <- delayedEval
  } yield value1 + value2

  val evalEx1 = for {
    a <- delayedEval
    b <- redoEval
    c <- instantEval
    d <- redoEval
  } yield a + b + c + d

  // now, later, again, again, sum
  // again, again, sum

  // remember a computed value
  val dontRcompute = redoEval.memoize

  val tutorial = Eval
    .always {
      println("step 1...");
      "put the guitar on your lap"
    }
    .map { step1 => println("step 2..."); s"$step1 then put your left hand on the neck" }
    .memoize // remember the value up to this point, calculate only once
    .map { steps12 => println("Step 3, more complicated"); s"$steps12 then with the right hand strike the strings" }

  def defer[T](eval: => Eval[T]): Eval[T] = Eval.later(()).flatMap(_ => eval)

  def reverseList[T](list: List[T]): List[T] =
    if (list.isEmpty) list
    else reverseList(list.tail) :+ list.head

  def reverseEval[T](list: List[T]): Eval[List[T]] =
    if (list.isEmpty) Eval.now(list)
    else Eval.defer(reverseEval(list.tail).map(_ :+ list.head))

  def main(args: Array[String]): Unit = {
    println(reverseEval(List(1, 2, 3, 4, 5)).value)
  }
}
