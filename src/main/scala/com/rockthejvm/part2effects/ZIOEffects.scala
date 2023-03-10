package com.rockthejvm.part2effects

import zio.*

import scala.annotation.tailrec

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

  /*
    Exercises
  */
  // TODO 1: sequence two ZIOs and take the value of the last one
  // This effect first evaluates zioA, and then evaluates, zioB, if zioB is successful, we return its value
  def sequenceTakeLast[R, E, A, B](zioA: ZIO[R, E, A], zioB: ZIO[R, E, B]): ZIO[R, E, B] =
    for {
      _ <- zioA
      resultB <- zioB
    } yield resultB

  def sequenceTakeLastZIO[R, E, A, B](zioA: ZIO[R, E, A], zioB: ZIO[R, E, B]): ZIO[R, E, B] = zioA *> zioB

  // TODO 2: sequence two ZIOs and take the value of the first one
  // This effect first evaluates zioA, and then evaluates, zioB, if zioB is successful, we return its value
  def sequenceTakeFirst[R, E, A, B](zioA: ZIO[R, E, A], zioB: ZIO[R, E, B]): ZIO[R, E, A] =
    for {
      resultA <- zioA
      _ <- zioB
    } yield resultA

  def sequenceTakeFirstZIO[R, E, A, B](zioA: ZIO[R, E, A], zioB: ZIO[R, E, B]): ZIO[R, E, A] = zioA <* zioB

  // TODO 3: run a ZIO forever
  def runForever[R, E, A](zio: ZIO[R, E, A]): ZIO[R, E, A] = zio.flatMap(_ => runForever(zio))
  // TODO: ZIO flatMaps implement a technique called trampolining for allocating ZIO instances on the heap instead of on the stack => ZIO runtime evaluates this tail recursively (ZIO flatMap returns immediately with a new data structure)
  def runForeverZIO[R, E, A](zio: ZIO[R, E, A]): ZIO[R, E, A] = zio *> runForeverZIO(zio)

  // TODO 4: convert the value of a ZIO to something else
  def convert[R, E, A, B](zio: ZIO[R, E, A], value: B): ZIO[R, E, B] = // zio.map(_ => value)
    for {
      _ <- zio
    } yield value

  def convertZIO[R, E, A, B](zio: ZIO[R, E, A], value: B): ZIO[R, E, B] = zio.as(value)

  // TODO 5: discard the value of a ZIO to Unit
  def asUnit[R, E, A](zio: ZIO[R, E, A]): ZIO[R, E, Unit] = // convertZIO(zio, ())
    for {
      _ <- zio
    } yield ()

  def asUnitZIO[R, E, A](zio: ZIO[R, E, A]): ZIO[R, E, Unit] = zio.unit

  // TODO 6: tail recursion (i.e. n = 4 => return 10)
  //def sum(n: Int): Int = if (n == 0) 0 else n + sum(n - 1)
  def sumZIO(n: Int): UIO[Int] = if (n <= 0) ZIO.succeed(0) else
    for {
      i <- ZIO.succeed(n)
      j <- sumZIO(i - 1)
    } yield i + j
  // ZIO tail recursion only applies to flatMap chains (not map by itself, i.e. single line for-comprehension)

  // TODO 7: Fibonacci sequence of ZIOs (hint: use ZIO.suspendSucceed to fix eventual StackOverflow errors and Throwable => Nothing)
  // a series of numbers in which each number ( Fibonacci number ) is the sum of the two preceding numbers
  def fibonacci(n: Int): BigInt = if (n <= 2) 1 else fibonacci(n - 1) + fibonacci(n - 2)
  def fibonacciZIO(n: Int): UIO[BigInt] =
    if (n <= 2) ZIO.succeed(1)
    else for {
      last <- ZIO.suspendSucceed(fibonacciZIO(n - 1)) // lazy effect that delays evaluation (like Cats Eval)
      previous <- fibonacciZIO(n - 2)
    } yield last + previous


  def main(args: Array[String]): Unit = {
    // ZIOs has guardrails to evaluate them (vs exposing unsafeRun directly)

    val runtime = Runtime.default // thread pool mechanism

    implicit val trace: Trace = Trace.empty // ability to debug ZIO applications on any thread

    Unsafe.unsafeCompat { implicit u: Unsafe =>
      val molEval = runtime.unsafe.run(meaningOfLife)
      println(molEval)
      // 1
      val firstEffect = runtime.unsafe.run(sequenceTakeLastZIO(aSuccessfulIO, aSuccessfulTask))
      println(firstEffect) // Success(89)
      // 2
      val secondEffect = runtime.unsafe.run(sequenceTakeFirstZIO(aSuccessfulIO, aSuccessfulTask))
      println(secondEffect) // Success(34)
      // 4
      val fourthEffect = runtime.unsafe.run(convertZIO(aUIO, "99 converted"))
      println(fourthEffect)// 99 => Success("99 converted")
      // 5
      val fifthEffect = runtime.unsafe.run(asUnitZIO(aUIO))
      println(fifthEffect) // 99 => Success(())
      // Interesting. If a ZIO fails, it returns the error channel wrapped.
      // The actual yield value is only returned (wrapped) in case of success(es)

      // 3
//      runtime.unsafe.run(runForeverZIO {
//        ZIO.succeed {
//          println("Running...")
//          Thread.sleep(1000)
//        }
//      })
      val sixthEffect = runtime.unsafe.run(sumZIO(100000))
      println(sixthEffect)
      //println(sum(100000))
      val seventhEffect = runtime.unsafe.run(fibonacciZIO(144))
      println(seventhEffect)
    }
  }
}
