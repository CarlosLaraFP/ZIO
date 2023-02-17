package com.rockthejvm.part4coordination

import zio._
import com.rockthejvm.utils._

object Promises extends ZIOAppDefault {

  /*
    TODO: Promise is a concurrency primitive for waiting for a ZIO while other ZIO completes it with a value
      - fully functional semantic block on a fiber until another fiber completes it with a value
      - the ability to wait on a value that may not be available yet without thread starvation
      - inter-Fiber communication
  */

  val promise: UIO[Promise[Throwable, Int]] = Promise.make[Throwable, Int]

  // await - block the Fiber until the Promise has a value
  val reader: Task[Int] = promise.flatMap { promise =>
    promise.await
  }

  // succeed, fail, complete (the fiber performing any of these unblocks the promise's fiber)
  val writer: UIO[Boolean] = promise.flatMap { promise =>
    Thread.sleep(5.seconds)
    promise.succeed(42) // unblocks reader fiber
  }

  val myDemo: Task[Unit] = for {
    p <- Promise.make[Throwable, Int]
    blockedFiber <- p.await.fork
    _ <- ZIO.sleep(5.seconds) *> p.succeed(42).unit.fork
    _ <- blockedFiber.join.debugThread
  } yield ()

  // producer - consumer problem
  def demoPromise: Task[Unit] = {
    def consumer(promise: Promise[Throwable, Int]): Task[Unit] =
      for {
        _ <- ZIO.succeed("[Consumer] waiting for result...").debugThread
        mol <- promise.await
        _ <- ZIO.succeed(s"[Consumer] got the result: $mol").debugThread
      } yield ()

    def producer(promise: Promise[Throwable, Int]) =
      for {
        _ <- ZIO.succeed("[Producer] crunching numbers...").debugThread
        _ <- ZIO.sleep(3.seconds)
        _ <- ZIO.succeed("[Producer] crunching complete").debugThread
        mol <- ZIO.succeed(42)
        _ <- promise.succeed(mol)
      } yield ()

    for {
      promise <- Promise.make[Throwable, Int]
      _ <- consumer(promise) zipPar producer(promise)
    } yield ()
  }

  // simulate downloading from multiple parts
  val fileParts = List("I ", "love S", "cala", " with pure FP an", "d ZIO! <EOF>")

  def downloadFileWithRef: UIO[Unit] = {
    def downloadFile(contentRef: Ref[String]): UIO[Unit] =
      ZIO.collectAllDiscard(
        fileParts.map { part =>
          ZIO.succeed(s"got '$part'").debugThread *>
            ZIO.sleep(1.second) *>
            contentRef.update(_ + part)
        }
      )

    def notifyFileComplete(contentRef: Ref[String]): UIO[Unit] =
      for {
        file <- contentRef.get
        _ <- if (file.endsWith("<EOF>")) ZIO.succeed("File download complete").debugThread
             else ZIO.succeed("Downloading...").debugThread *> ZIO.sleep(500.millis) *> notifyFileComplete(contentRef)
      } yield ()

    for {
      contentRef <- Ref.make("")
      _ <- downloadFile(contentRef) zipPar notifyFileComplete(contentRef)
    } yield ()
  }


  override def run = downloadFileWithRef

}
