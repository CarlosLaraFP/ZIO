package com.rockthejvm.part2effects

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.Try


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

  def measure[A](computation: MyIO[A]): MyIO[(Long, A)] =
    for {
      start <- getCurrentTime
      result <- computation
      end <- getCurrentTime
    } yield (end - start, result)

  def readFromConsole: MyIO[String] = MyIO(() => scala.io.StdIn.readLine )

  def display(text: String): MyIO[Unit] = MyIO(() => println(text))

  def userInteraction: MyIO[Unit] =
    for {
      _ <- display("What is your name?")
      userInput <- readFromConsole
      _ <- display(s"Welcome to ZIO, $userInput!")
    } yield ()

  def identicalMeasure[A](computation: MyIO[A]): MyIO[(Long, A)] =
    getCurrentTime flatMap { start =>
      computation flatMap { result =>
        getCurrentTime map { end =>
          (end - start, result)
        }
      }
    }

  def referentiallyTransparentMeasure[A](computation: MyIO[A]): MyIO[(Long, A)] =
    MyIO { () =>
      val start = getCurrentTime.unsafeRun()
      val result = computation.unsafeRun()
      val end = getCurrentTime.unsafeRun()
      (end - start, result)
    }

  /*
    A simplified ZIO

    Try is a wrapper over a computation that might fail with an exception (only java.lang.Throwable)
    Either is more powerful because the first type parameter represents ANY error type, even domain-specific.
  */
  case class MyZIO[R, E, A](unsafeRun: R => Either[E, A]) {
    def map[B](f: A => B): MyZIO[R, E, B] =
      MyZIO(r => unsafeRun(r) match {
        case Left(e) => Left(e)
        case Right(v) => Right(f(v))
      })

    def flatMap[B](f: A => MyZIO[R, E, B]): MyZIO[R, E, B] =
      MyZIO(r => unsafeRun(r) match {
        case Left(e) => Left(e)
        case Right(v) => f(v).unsafeRun(r)
      })
  }


  def main(args: Array[String]): Unit = {
    //
    anIOWithSideEffects.unsafeRun()
    // 1
    println(getCurrentTime.unsafeRun())
    // 2
    val measurement = measure(MyIO(() => {
      Thread.sleep(1000)
      "Slept for 1 second"
    })).unsafeRun()
    println(s"Total duration: ${measurement._1} milliseconds")
    println(s"Value returned from 2nd computation: ${measurement._2}")
    // 3
    println(readFromConsole.unsafeRun())
    // 4
    userInteraction.unsafeRun()
  }
}
