package com.rockthejvm.part3concurrency

import zio._

import java.util.Scanner
import java.io.File

import com.rockthejvm.utils._

object Resources extends ZIOAppDefault {

  // TODO: Finalizers are used to manage the lifecycle of a resource
  def unsafeMethod: Int = throw new RuntimeException("No Int here...")
  val attempt: Task[Int] = ZIO.attempt(unsafeMethod)

  // finalizers (attached to a ZIO and run regardless of outcome)
  val attemptWithFinalizer: Task[Int] =
    attempt.ensuring(ZIO.succeed("Finalizer!").debugThread)

  // multiple finalizers (run in order before the final effect finishes)
  val anotherAttempt: Task[Int] =
    attemptWithFinalizer.ensuring(ZIO.succeed("Another finalizer!").debugThread)

  // specialized finalizers (onDone, onError, onInterrupt, onExit)

  class Connection(url: String) {
    def open: UIO[String] = ZIO.succeed(s"Opening connection to $url...").debugThread
    def close: UIO[String] = ZIO.succeed(s"Closing connection to $url...").debugThread
  }
  object Connection {
    def create(url: String): UIO[Connection] = ZIO.succeed(new Connection(url))
  }

  val fetchUrl: UIO[Unit] =
    for {
      conn <- Connection.create("rockthejvm.com")
      fib <- (conn.open *> ZIO.sleep(300.seconds)).fork
      _ <- ZIO.sleep(1.second) *> ZIO.succeed("Interrupting").debugThread *> fib.interrupt
      _ <- fib.join
    } yield () // resource leak because connection was not closed

  // Solution: Finalizer
  val properFetchUrl: UIO[Unit] =
    for {
      conn <- Connection.create("rockthejvm.com")
      fib <- (conn.open *> ZIO.sleep(300.seconds)).ensuring(conn.close).fork
      _ <- ZIO.sleep(1.second) *> ZIO.succeed("Interrupting").debugThread *> fib.interrupt
      _ <- fib.join
    } yield () // prevents leaking connections

  // ensuring is low-level and error-prone at scale

  /*
    TODO: acquireRelease instead to specify how to acquire and release a resource
      - acquiring cannot be interrupted
      - all finalizers are guaranteed to run
  */
  val cleanConnection = ZIO.acquireRelease(Connection.create("rockthejvm.com"))(_.close)
  val fetchWithResource = for {
    conn <- cleanConnection
    fib <- (conn.open *> ZIO.sleep(300.seconds)).fork
    _ <- ZIO.sleep(1.second) *> ZIO.succeed("Interrupting").debugThread *> fib.interrupt
    _ <- fib.join
  } yield ()

  // removes Scope dependency from effect
  val fetchScoped: UIO[Unit] = ZIO.scoped(fetchWithResource)

  // acquireReleaseWith (because it includes usage, Scope dependency is not required)
  val cleanerConnection: UIO[Unit] =
    ZIO.acquireReleaseWith(
      Connection.create("rockthejvm.com") // acquisition
    )(
      _.close // release
    )(
      conn => conn.open *> ZIO.sleep(300.seconds) // usage
    )

  val fetchWithAnotherResource: UIO[Unit] =
    for {
      fib <- cleanerConnection.fork
      _ <- ZIO.sleep(1.second) *> ZIO.succeed("Interrupting").debugThread *> fib.interrupt
      _ <- fib.join
    } yield ()

  // TODO 1: Use acquireRelease to open a file and print all lines (one every 100 millis), then close file
  def openFileScanner(path: String): UIO[Scanner] = // scanner.hasNext or .nextLine
    ZIO.succeed(new Scanner(new File(path)))

  def scanFile(scanner: Scanner): UIO[Unit] = {
      println(scanner.nextLine())
      ZIO.sleep(100.millis) *> scanFile(scanner)
    }

  def acquireOpenFile(path: String): UIO[Unit] =
    ZIO.acquireReleaseWith(
      ZIO.succeed("Starting file scan...") *> openFileScanner(path)
    )(
      scanner =>
        ZIO.succeed("Closing Scanner...").debugThread *>
          ZIO.succeed(scanner.close()) *>
            ZIO.succeed("Scanner closed").debugThread
    )(
      scanFile
    )

  val testInterruptFileDisplay: UIO[Unit] =
    for {
      fib <- acquireOpenFile("src/main/scala/com/rockthejvm/part3concurrency/Resources.scala")
        .debugThread
        .fork
      _ <- ZIO.sleep(2.seconds) *> fib.interrupt
    } yield ()


  override def run = testInterruptFileDisplay
}
