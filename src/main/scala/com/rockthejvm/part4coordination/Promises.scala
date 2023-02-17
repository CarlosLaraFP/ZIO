package com.rockthejvm.part4coordination

import zio._
import com.rockthejvm.utils._

object Promises extends ZIOAppDefault {

  // Promise is a primitive for waiting for a ZIO while other ZIO completes it with a value
  // blocked until another fiber/thread completes it with a value

  val promise: UIO[Promise[Throwable, Int]] = Promise.make[Throwable, Int]

  // await - block the Fiber until the Promise has a value
  val reader: Task[Int] = promise.flatMap { promise =>
    promise.await
  }

  // succeed, fail, complete
  val writer: UIO[Boolean] = promise.flatMap { promise =>
    Thread.sleep(5.seconds)
    promise.succeed(42) // unblocks reader fiber
  }


  override def run =
    for {
      promise <- Promise.make[Throwable, Int]
      blockedFiber <- promise.await.fork
      _ <- ZIO.sleep(5.seconds) *> promise.succeed(42).unit.fork
      _ <- blockedFiber.join.debugThread
    } yield ()
}
