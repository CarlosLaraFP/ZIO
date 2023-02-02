package com.rockthejvm.part3concurrency

import zio._
import com.rockthejvm.utils._

object Interruptions extends ZIOAppDefault {

  val zioWithTime: UIO[Int] = {
    ZIO.succeed("Starting computation").debugThread *>
      ZIO.sleep(2.seconds) *>
        ZIO.succeed(42).debugThread
  }
    .onInterrupt { // callback guarding against interruption exceptions
      ZIO.succeed("Fiber interrupted").debugThread
    }
  // .onDone callback for graceful shutdowns in case of successful completion

  val interruption: Task[Int] =
    for {
      fiber <- zioWithTime.fork
      _ <- ZIO.sleep(1.second) *> ZIO.succeed("Interrupting!").debugThread *> fiber.interrupt
      _ <- ZIO.succeed("Interruption successful.").debugThread
      result <- fiber.join
    } yield result
  // the fiber.interrupt effect that interrupts the handler of a Fiber by sending a signal to it to be intercepted by the ZIO runtime and stop evaluating the fiber
  // the fiber.interrupt effect will block the calling fiber (entire line 20 effect) until the interrupted fiber has successfully been interrupted (or happened to finish first)

  /* TODO:
      If you do not want interruptions to block the calling fiber, fork it to make it asynchronous (run on a different fiber).
      This new Fiber gets itself blocked, but it doesn't matter because the main for-comprehension continues.
      Since we do not join the fiber, it's technically a leaked resource (cheap because they are simple data structures stored on the heap),
      but if the [thread/process] lifecycle is known to be very short, and the result is not referenced anywhere,
      the JVM garbage collector will clean it up.
  */
  val asyncInterruption: Task[Int] =
    for {
      fiber <- zioWithTime.fork
      _ <- ZIO.sleep(1.second) *> ZIO.succeed("Interrupting!").debugThread *> fiber.interruptFork
      _ <- ZIO.succeed("Interruption successful.").debugThread
      result <- fiber.join
    } yield result

  // Automatic interruptions
  // outliving a parent fiber
  val parentEffect: ZIO[Any, Any, String] =
    ZIO.succeed("Spawning Fiber").debugThread *>
      //zioWithTime.fork *> // child fiber (and longer than the parent effect)
      zioWithTime.forkDaemon *> // the fiber will now be a child of the MAIN application fiber
        ZIO.sleep(1.second) *>
          ZIO.succeed("Parent successful").debugThread // done here

  // child fibers will be automatically interrupted if the parent fiber completes for any reason
  val testOutlivingParent: ZIO[Any, Any, Unit] =
    for {
      parentFiber <- parentEffect.fork
      _ <- ZIO.sleep(3.seconds)
      _ <- parentFiber.join
    } yield ()

  // racing: ZIO feature that spin up 2 fibers running effects in parallel
  // the first one that finishes is the winner of the race and the loser (longer) will be automatically interrupted
  val slowEffect: UIO[String] = {
    ZIO.sleep(2.seconds) *>
      ZIO.succeed("Slow").debugThread
  }.onInterrupt {
    ZIO.succeed("[Slow] interrupted").debugThread
  }

  val fastEffect: UIO[String] = {
    ZIO.sleep(1.second) *>
      ZIO.succeed("Fast").debugThread
  }.onInterrupt {
    ZIO.succeed("[Fast] interrupted").debugThread
  }

  val race: UIO[String] = slowEffect.race(fastEffect)

  val testRace: UIO[Unit] = race.fork *> ZIO.sleep(3.seconds)

  // TODO 1: Implement timeout function
  //  - if zio completes with A before time runs out, the final effect completes with the value;
  //  - if it fails with E, effect completes with e
  //  - if zio takes longer than the allocated time, it will be interrupted
  def timeout[R, E, A](zio: ZIO[R, E, A], time: Duration): ZIO[R, E, A] =
    for {
      taskFiber <- zio.fork
      _ <- ZIO.sleep(time).onDone(ZIO.succeed(""), _ => taskFiber.interruptFork).forkDaemon
      result <- taskFiber.join
    } yield result


  override def run: ZIO[Any, Any, Any] =
    for {
      resultA <- timeout(ZIO.succeed("Success A"), 2.seconds)
      _ <- Console.printLine(resultA)
      resultB <- timeout(ZIO.fail("B failed"), 2.seconds).catchAll(e => ZIO.succeed(e))
      _ <- Console.printLine(resultB)
      resultC <- timeout(ZIO.sleep(3.seconds), 2.seconds)
      _ <- Console.printLine(resultC)
    } yield ()
}
