package com.rockthejvm.part3concurrency

import zio._
import com.rockthejvm.utils._

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

  override def run: ZIO[Any, Any, Any] = chainedFibers.debugThread
}
