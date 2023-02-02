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
      _ <- (ZIO.sleep(time) *> taskFiber.interruptFork).fork//Daemon
      result <- taskFiber.join
    } yield result

  // TODO 2: Timeout V2 -> if zio takes longer than the allocated time, return zio with nothing for the value channel
  //  - successful effect with Some(a)
  //  - failed effect with e
  //  - interrupt effect and return successful effect with None
  def timeoutOption[R, E, A](zio: ZIO[R, E, A], time: Duration): ZIO[R, E, Option[A]] =
    timeout(zio, time).foldCauseZIO(
      cause => if (cause.isInterrupted) ZIO.succeed(None) else ZIO.failCause(cause),
      value => ZIO.succeed(Some(value))
    )
  /*
    val timeoutEffect: ZIO[R, E, Option[A]] = ZIO.sleep(time) *> ZIO.succeed(Option.empty[A])

    // zio.option absorbs the failure
    val taskEffect: ZIO[R, E, Option[A]] =
      zio.foldCause(c => {
        println((c.failures.map(_.toString) ::: c.defects.map(_.toString)).mkString(", "))
        None
      }, a => Some(a))

    timeoutEffect.race(taskEffect)
  */


  override def run: ZIO[Any, Any, Any] =
    timeoutOption(
      ZIO.succeed("Starting...").debugThread *> ZIO.sleep(1.seconds) *> ZIO.succeed("Success A").debugThread,
      2.seconds
    ).debugThread
}
