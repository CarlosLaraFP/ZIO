package com.rockthejvm.part1recap

import java.util.concurrent.Executors
import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.util.{Success, Failure, Try}

object Essentials {

  // values
  val aBoolean: Boolean = false

  // expressions are EVALUATED to a value
  val anIfExpression: String = if (2 > 3) "bigger" else "smaller"

  // instructions involve side-effects and 'return' Unit ()
  val theUnit: Unit = println("Hello, Scala")

  // OOP
  class Animal
  class Cat extends Animal
  trait Carnivore {
    def eat(animal: Animal): Unit
  }

  // single inheritance + multiple mix-in traits model
  class Crocodile extends Animal with Carnivore {
    override def eat(animal: Animal): Unit = println("Crunch!")
  }

  // singleton pattern
  object MySingleton

  // companion objects (same file) for static methods
  object Carnivore

  // generics
  class MyList[A]

  // method notation
  val three: Int = 1 + 2 // infix
  val anotherThree: Int = 1.+(2)

  // functional programming
  val incrementer: Int => Int = x => x + 1
  val incremented: Int = incrementer(45) // 46

  // higher order functions (FP)
  // map, flatMap, filter
  val processedList: List[Int] = List(1, 2, 3).map(incrementer) // List(2, 3, 4)
  val aLongerList: List[Int] = List(1, 2, 3).flatMap(x => List(x, x + 1)) // List(1, 2, 2, 3, 3, 4)

  // for-comprehensions
  val checkerBoard: List[(Int, Char)] = List(1, 2, 3).flatMap(n => List('a', 'b', 'c').map(c => (n, c))) // Cartesian product

  // syntactic sugar for flatMap + map
  val checkerBoardFor: List[(Int, Char)] = for {
    n <- List(1, 2, 3)
    c <- List('a', 'b', 'c')
  } yield (n, c)

  // options and try
  val anOption: Option[Int] = Option(3) // apply method of Option companion returns subtype Some or None
  val doubledOption: Option[Int] = anOption.map(_ * 2) // works regardless of Some or None

  // something that might throw
  val anAttempt: Try[Int] = Try(42) // Success(value) or Failure(exception)
  val modifiedAttempts: Try[Int] = anAttempt.map(_ + 10) // works regardless of Success or Failure

  // pattern matching
  val anUnknown: Any = 45
  val ordinal: String = anUnknown match {
    case 1 => "first"
    case 2 => "second"
    case _ => "unknown"
  }

  val anOptionDescription: String = anOption match {
    case Some(x) => s"the option is not empty: $x"
    case None => "the option is empty"
  }

  // Futures for asynchronous programming using multiple threads (concurrency)
  //implicit scala.concurrent.ExecutionContext.Implicits.global // Scala 2.12 (downsides)
  implicit val ec: ExecutionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(8))

  val aFuture: Future[Int] = Future {
    42
  }
  // wait for completion (async) - onComplete is also evaluated on another thread
  aFuture onComplete {
    case Success(x) => println(s"The async meaning of life is: $x")
    case Failure(e) => println(s"Meaning of value failed: $e")
  }

  // map a Future
  val anotherFuture: Future[Int] = aFuture.map(_ + 1) // Future(43) when it completes

  // partial functions
  val aPartialFunction: PartialFunction[Int, Int] = {
    case 1 => 43
    case 8 => 56
    case 100 => 999
  }

  // some more advanced bits
  trait HigherKindedType[F[_]]

  trait SequenceChecker[F[_]] {
    def isSequential: Boolean
  }

  val listChecker: SequenceChecker[List] = new SequenceChecker[List] {
    override def isSequential: Boolean = true
  }


  def main(args: Array[String]): Unit = {

  }
}
