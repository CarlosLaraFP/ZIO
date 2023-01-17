package com.rockthejvm.part1recap

import java.util

object Variance {
  /*
    The ZIO family of libraries embraces OOP and subtyping for generics (variance).

    OOP - substitution
    Variance is the extension of the substitution model to generic types.
  */

  class Animal
  class Dog(name: String) extends Animal

  val ellie = new Dog("Ellie")
  val anAnimal: Animal = ellie

  // Variance question for List: if Dog <: Animal, then should List[Dog] <: List[Animal]? YES == COVARIANT
  val hachi = new Dog("Hachi")
  val laika = new Dog("Laika")
  val animals: List[Animal] = List(ellie, hachi, laika)

  class MyList[+A] // MyList is covariant in generic type parameter A
  val myAnimalList: MyList[Animal] = new MyList[Dog]

  // NO == INVARIANT (no subtype relationship)
  trait Semigroup[A] {
    def combine(x: A, y: A): A
  }

  // all generics in Java are invariant
  //val aJavaList: java.util.ArrayList[Animal] = new util.ArrayList[Dog]()

  // HELL NO == CONTRAVARIANT
  trait Vet[-A] {
    def heal(animal: A): Boolean
  }

  // Vet[Animal] is "better" than a Vet[Dog]
  // Dog <: Animal, then Vet[Dog] >: Vet[Animal]
  val myVet: Vet[Dog] = new Vet[Animal] {
    override def heal(animal: Animal): Boolean = {
      println("Healing...")
      true
    }
  }


  def main(args: Array[String]): Unit = {
    //
    myVet.heal(ellie)
  }
}
