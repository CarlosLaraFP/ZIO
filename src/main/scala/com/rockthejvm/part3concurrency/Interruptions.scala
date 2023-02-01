package com.rockthejvm.part3concurrency

import zio._
import com.rockthejvm.utils._

object Interruptions extends ZIOAppDefault {

  val zioWithTime: UIO[Int] =
    ZIO.succeed("Starting computation").debugThread *>
      ZIO.sleep(2.seconds) *>
        ZIO.succeed(42).debugThread

  val interruption: UIO[Int] =
    for {
      fiber <- zioWithTime.fork
      _ <- ZIO.sleep(1.second) *> ZIO.succeed("Interrupting!").debugThread *> fiber.interrupt // interrupts the handler of a Fiber by sending a signal to it to be intercepted by the ZIO runtime and stop evaluating the fiber
      _ <- ZIO.succeed("Interruption successful.").debugThread
      result <- fiber.join
    } yield result


  override def run: ZIO[Any, Any, Any] =
    for {
      result <- interruption
      _ <- Console.printLine(result)
    } yield ()
}
