package com.rockthejvm.part3concurrency

import java.io.{File, FileReader, FileWriter}

import zio.*
import com.rockthejvm.utils.*
import cats.Semigroup
import cats.syntax.semigroup._


object Fibers extends ZIOAppDefault {

  // Normally, these effects execute synchronously on the same thread when using combinators (i.e. for-comprehensions)
  val meaningOfLife: UIO[Int] = ZIO.succeed(42)
  val favoriteLanguage: UIO[String] = ZIO.succeed("Scala")

  /*
    TODO: Fiber == lightweight thread on native JVM
      - description of a computation that will be performed by one of the threads managed by the ZIO runtime
      - we use ZIO APIs to start Fibers automatically and schedule them on the thread pool managed by the ZIO runtime
      - zio.fork produces an effect whose value is a Fiber
      (forking an effect creates another effect whose return value is the Fiber on which that effect will be evaluated)
      - Fibers can be joined (fork + join paradigm from regular threads)
      - fiber.join returns another effect which will block until the Fiber has been completed
   */
  def createFiber: Fiber[Throwable, String] = ??? // practically impossible to create manually

  val sameThreadIO: UIO[(Int, String)] = for {
    mol <- meaningOfLife.debugThread
    lang <- favoriteLanguage.debugThread
  } yield (mol, lang)


  val differentThreadIO: UIO[Unit] = for {
    _ <- meaningOfLife.debugThread.fork // effectful execution is separate from ZIO effect instantiation
    _ <- favoriteLanguage.debugThread.fork
  } yield ()

  val meaningOfLifeFiber: UIO[Fiber[Throwable, Int]] = meaningOfLife.fork

  // join a Fiber
  def runOnAnotherThread[R, E, A](zio: ZIO[R, E, A]): ZIO[R, E, A] = for {
    fib <- zio.fork
    result <- fib.join // waits for Fiber to complete
  } yield result

  // awaiting a Fiber
  def runOnDifferentThread[R, E, A](zio: ZIO[R, E, A]): ZIO[R, Nothing, String] =
    for {
      fib <- zio.fork
      result <- fib.await
    } yield result match {
      case Exit.Success(value) => s"Succeeded with $value"
      case Exit.Failure(cause) => s"Failed with $cause"
    }

  // poll (peek at the result of the fiber RIGHT NOW, without blocking)
  val peekFiber: ZIO[Any, Nothing, Option[Exit[Throwable, Int]]] = for {
    fib <- ZIO.attempt {
      Thread.sleep(1000)
      42
    }.fork
    result <- fib.poll
  } yield result

  // TODO: compose Fibers

  // zip
  val zippedFibers: UIO[(String, String)] = for {
    fibA <- ZIO.succeed("Result from Fiber A").debugThread.fork
    fibB <- ZIO.succeed("Result from Fiber B").debugThread.fork
    fiber = fibA zip fibB // zip does not return a ZIO effect, but an actual Fiber value
    tuple <- fiber.join
  } yield tuple

  // orElse
  val chainedFibers: UIO[String] = for {
    fiberA <- ZIO.fail("not good").debugThread.fork
    fiberB <- ZIO.succeed("success!").debugThread.fork
    fiber = fiberA orElse fiberB
    message <- fiber.join
  } yield message

  // Exercises

  def testFibers(debug: String): UIO[(Fiber[Nothing, String], Fiber[Nothing, String])] =
    for {
      fiberA <- ZIO.succeed(s"$debug: First").debugThread.fork
      fiberB <- ZIO.succeed(s"$debug: Second").debugThread.fork
    } yield (fiberA, fiberB)

  // TODO 1: zip two Fibers using fork and join, without the zip combinator (hint: create a fiber that waits for both)
  // the below implementation is temporary and not ideal (interruptions, synchrony, ...)
  def zipFibers[E, X <: E, Y <: E, A, B](fiberA: Fiber[X, A], fiberB: Fiber[Y, B]): UIO[Fiber[E, (A, B)]] = {
    val tupleEffect = for {
      a <- fiberA.join
      b <- fiberB.join
    } yield (a, b)

    tupleEffect.debugThread.fork
  }

  // TODO 2: same as above, but with orElse
  def chainFibers[E, A](fiberA: Fiber[E, A], fiberB: Fiber[E, A]): UIO[Fiber[E, A]] =
    fiberA
      .join
      .orElse(fiberB.join)
      .fork

  // TODO 3: distributing tasks in between many fibers
  def generateRandomFile(path: String): Unit = {
    val random = scala.util.Random
    val chars = 'a' to 'z'
    val nWords = random.nextInt(2000) // at most 2000 random words
    val content = (1 to nWords)
      .map(_ =>
        (1 to random.nextInt(10)) // at most 10 characters per word
          .map(_ => chars(random.nextInt(26)))
          .mkString
      ) // one word for every 1 to nWords
      .mkString(" ")

    val writer = new FileWriter(new File(path))
    writer.write(content)
    writer.flush()
    writer.close()
    /*
      When data is written to a file, it may not be immediately written to the physical storage device.
      Instead, it may be temporarily stored in a buffer, or a temporary holding area in memory.
      "Flushing" the buffer means to write any data that is currently stored in the buffer to the actual file on disk,
      so that it can be accessed by other processes. This can be important for ensuring that data is not lost in the event of a
      crash or power failure, as well as for allowing other processes to read the file as soon as possible after it is written.
    */
  }
  // TODO 3: Use 10 Fibers to count the number of words in all the generated files and then aggregate all the results
  // TODO classic MapReduce problem: Spawn N Fibers, count the N of words in each file, then aggregate all the results in one big number
  def countWords(filePath: String): UIO[Int] =
    ZIO.succeed {
      val source = scala.io.Source.fromFile(filePath)
      val nWords = source.getLines.toList.head.split(" ").count(_.nonEmpty)
      source.close
      nWords
    }

  implicit val zioSemigroup: Semigroup[UIO[Int]] = Semigroup.instance[UIO[Int]] {
    // Define custom combination
    (zioA, zioB) =>

      val combinedFiberEffect: UIO[Fiber[Nothing, (Int, Int)]] =
        for {
          a <- zioA.fork
          b <- zioB.fork
        } yield a zip b

      val combinedEffect: UIO[Int] =
        for {
          combinedFiber <- combinedFiberEffect
          tupleResult <- combinedFiber.join
        } yield tupleResult._1 + tupleResult._2

      combinedEffect
  }
  
  // cats.Semigroup to make code more concise
  def wordCountParallel(n: Int): UIO[Int] = {
    (1 to n).toList
      .map(i => s"src/main/resources/testfile_$i.txt") // paths
      .map(countWords) // list of effects
      .reduce(_ |+| _)
      //.map(_.fork) // list of effects returning fibers
      //.map((fiberEffect: UIO[Fiber[Nothing, Int]]) => fiberEffect.flatMap(_.join)) // list of effects returning values (count of words)
      //.reduce((effectA, effectB) => for {
      //  countA <- effectA
      //  countB <- effectB
      //} yield countA + countB)
  }


  override def run: ZIO[Any, Any, Any] =

    val tests = for {
      fibersOne <- testFibers("zipFibers")
      zippedFiber <- zipFibers(fibersOne._1, fibersOne._2)
      fibersTwo <- testFibers("chainFibers")
      chainedFiber <- chainFibers(fibersTwo._1, fibersTwo._2)
      summedWords <- wordCountParallel(10)
    } yield (summedWords, zippedFiber.join)

    tests.debugThread

  //ZIO.succeed((1 to 10).foreach(i => generateRandomFile(s"src/main/resources/testfile_$i.txt")))
}
