package com.rockthejvm.part2effects

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future


object Effects {
  /*
    Functional Programming

    Always think in terms of EXPRESSIONS: Structures that evaluate to a value.
  */
  def combine(a: Int, b: Int): Int = a + b
  // Local Reasoning == the type signature describes the entire computation that will be performed

  // Referential Transparency == the ability to replace an expression with the value that it evaluates to without changing the behavior of the program
  val five: Int = combine(2, 3)
  val anotherFive: Int = 2 + 3
  val moreFive: Int = 5

  // not all expressions are referentially transparent
  val result: Unit = println("Learning ZIO")
  val anotherResult: Unit = () // the return values are the same, but the behavior is different

  var anInt: Int = 0
  val changingInt: Unit = (anInt = 42) // side effect
  val changingAgain: Unit = () // not the same program

  /*
    Takeaway: We cannot safely replace an expression that produces a side-effect with the value that it evaluates to.

    Solution: Effect => data structure properties:
      - the type signature describes what kind of computation it will perform
      - the type signature describes the type of VALUE that it will produce
      - if side effects are required, the construction of the data structure must be separate from the EXECUTION
  */

  // Option satisfies all 3 Effect properties. Therefore, Option is an Effect.
  val anOption: Option[Int] = Option(42)

  // Future (asynchronous computation on another JVM thread that will finish at some point in the future) meets 2 / 3
  // allocates a JVM thread and scheduling the computation on top of that JVM thread
  val aFuture: Future[Int] = Future(42)

  /*
    Monadic IO (from the advanced Scala 3 course)
    - describes any computation that might perform side-effects
    - produces values of type A if the computation is successful
    - side-effects are required and construction is separate from execution

    MyIO is the most general Effect
  */
  case class MyIO[A](unsafeRun: () => A) {
    def map[B](f: A => B): MyIO[B] = MyIO(() => f(unsafeRun()))
    def flatMap[B](f: A => MyIO[B]): MyIO[B] = MyIO(() => f(unsafeRun()).unsafeRun())
  }

  // MyIO instantiated, but the function has not been evaluated yet
  val anIOWithSideEffects: MyIO[Int] = MyIO(() => {
    println("Producing effect")
    42
  })

  /*
    Exercises
    TODO 1: Create an IO to measure the current time of the system
    TODO 2: Create an IO to measure the duration of a computation (use 1 and map/flatMap combinations of MyIO)
    TODO 3: Create an IO to read something from the console
    TODO 4: Print something to the console, then read, then print a welcome message
  */
  def getCurrentTime: MyIO[Long] = MyIO(() => {
    println(java.util.Calendar.getInstance().getTimeZone.getDisplayName)
    java.util.Calendar.getInstance().getTimeInMillis
  })

  def measure[A](computation: MyIO[A]): MyIO[(Long, A)] = ???


  def main(args: Array[String]): Unit = {
    //
    anIOWithSideEffects.unsafeRun()
    val currentTime = getCurrentTime.unsafeRun()
    println(currentTime)
  }
}
