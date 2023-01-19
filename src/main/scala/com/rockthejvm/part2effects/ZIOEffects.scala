package com.rockthejvm.part2effects

import zio._

object ZIOEffects {
  /*
    A ZIO[R, E, A] value is an immutable value (called an "effect") that describes an async, concurrent workflow.
    In order to be executed, the workflow requires a value of type ZEnvironment[R], and when executed,
    the workflow will either produce a failure of type E, or a success of type A.

    ZIO effects may informally be thought of as functions of the following form:

     ZEnvironment[R] => Either[E, A]

    ZIO effects model resourceful interaction with the outside world, including synchronous, asynchronous, concurrent, and parallel interaction.
    The async and concurrent operations of ZIO effects are powered by fibers, which are lightweight, green threads that enable high scalability.
    To run an effect, you need a Runtime, which is capable of executing effects.
    Runtimes bundle a thread pool together with the environment that effects need.
  */

  // success
  val meaningOfLife: ZIO[Any, Nothing, Int] = ZIO.succeed(42)
  // failure
  val aFailure: ZIO[Any, String, Nothing] = ZIO.fail("Something went wrong")
  // suspension/delay
  val aSuspendedZIO: ZIO[Any, Throwable, Int] = ZIO.suspend(meaningOfLife)

  // map + flatMap
  val improved: ZIO[Any, Nothing, Int] = meaningOfLife.map(_ * 2)
  val printing: ZIO[Any, Nothing, Unit] = meaningOfLife.flatMap(v => ZIO.succeed(println(v)))
  // for-comprehensions
  val smallProgram: ZIO[Any, Nothing, Unit] = for {
    _ <- ZIO.succeed(println("What is your name?"))
    name <- ZIO.succeed(scala.io.StdIn.readLine())
    _ <- ZIO.succeed(println(s"Welcome to ZIO, $name"))
  } yield ()

  // A LOT of combinators and transformers

  // zip, zipWith
  val another: ZIO[Any, Nothing, Int] = ZIO.succeed(100)
  val tupledZIO: ZIO[Any, Nothing, (Int, Int)] = meaningOfLife.zip(another)
  val combineZIO: ZIO[Any, Nothing, Int] = meaningOfLife.zipWith(another)(_ * _)

  /*
    Type aliases are very useful and common due to the number of type parameters in ZIO
    Covariant type parameters allow replacing a subtype with a supertype (i.e. Nothing -> Throwable]
    Variance allows the type substitution via type aliases.
  */
  // UIO[A] == ZIO[Any, Nothing, A] - no requirements, cannot fail, produces A
  val aUIO: UIO[Int] = ZIO.succeed(99)

  // URIO[R, A] == ZIO[R, Nothing, A] - cannot fail but has requirements
  val aURIO: URIO[Int, Int] = ZIO.succeed(67)

  // RIO[R, A] == ZIO[R, Throwable, A] - has requirements and can fail with a Throwable
  val anRIO: RIO[Int, Int] = ZIO.succeed(98)
  val aFailedRIO: RIO[Int, Int] = ZIO.fail(new RuntimeException("RIO failed"))

  // Task[A] == ZIO[Any, Throwable, A] - no requirements, can fail with a Throwable, produces A
  val aSuccessfulTask: Task[Int] = ZIO.succeed(89)
  val aFailedTask: Task[Int] = ZIO.fail(new RuntimeException("Something bad"))
  /*
    List[+A]
    val cats: List[Animal] = List(Cat(...))
    Nothing is a subtype of all types, also called the bottom type. There is no value that has type Nothing.
    A common use is to signal non-termination such as a thrown exception, program exit, or an infinite loop (i.e., it is the type of an expression which does not evaluate to a value, or a method that does not return normally).
  */

  // IO[E, A] == ZIO[Any, E, A] - no requirements
  val aSuccessfulIO: IO[String, Int] = ZIO.succeed(34)
  val aFailedIO: IO[String, Int] = ZIO.fail("Something bad happened")


  def main(args: Array[String]): Unit = {
    //
  }
}
