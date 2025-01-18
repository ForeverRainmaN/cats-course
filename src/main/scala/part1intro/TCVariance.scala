package part1intro

import cats.implicits.catsSyntaxEq


object TCVariance {

  val aComparison = Option(2) == Option(3)

  // val invalidComparison = Some(2) === None // Eq[Some[Int]] not found
  // variance
  class Animal

  class Cat extends Animal

  // covariant type: sybtyping is propagated to the generic type
  class Cage[+T]

  val cage: Cage[Animal] = new Cage[Cat] // Cat <: Animal, so Cage[Cat] <: Cage[Animal]

  // contravariant type: subtyping is propagated BACKWARDS to the generic type
  class Vet[-T]

  val vet: Vet[Cat] = new Vet[Animal] // Cat <: Animal, then Vet[Animal] <: Vet[Cat]

  // rule of thumb: "HAS A T" = covariant
  // "ACTS on T" = contravariant
  // variance affect how TC instances are being fetched

  // contravariant TC
  trait SoundMaker[-T]

  implicit object AnimalSoundMaker extends SoundMaker[Animal]

  def makeSound[T](implicit soundMaker: SoundMaker[T]): Unit = println("WOW") // implementation not important

  makeSound[Animal] // ok - TC instance defined above
  makeSound[Cat] // ok - TC instance for Animal is also applicable to Cats
  // rule1: contravariant TCs can uuse the superclass instances if nothing is available strictly for that type

  // has implications for subtypes
  implicit object OptionSoundMaker extends SoundMaker[Option[Int]]

  makeSound[Option[Int]]
  makeSound[Some[Int]]


  // covariant TC
  trait AnimalShow[+T] {
    def show: String
  }

  implicit object GeneralAnimalShow extends AnimalShow[Animal] {
    override def show: String = "animals everywhere"
  }

  implicit object CatsShow extends AnimalShow[Cat] {
    override def show: String = "so many cats"
  }

  def organizeShow[T](implicit event: AnimalShow[T]): String = event.show

  // rule 2: covariant TCs will always use(prefer) the more specific TC instance for that type
  // but may confuse the compiler if the general TC is also present
  // rule3: you can't have both
  // Cats uses INVARIANT TCs

  // ==================
  //  Contravariant TCS can use the superclass TC instance if nothing available strictly for that type
  //  Covariant TCs will always use the more specific instance
  // ==================
  Option(2) === Option.empty[Int]

  def main(args: Array[String]): Unit = {
    println(organizeShow[Cat]) // ok - the compiler will inject CatsShow as implicit
    //    println(organizeShow[Animal]) // will not compile - ambiguous values
  }
}
