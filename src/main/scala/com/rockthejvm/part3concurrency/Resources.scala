package com.rockthejvm.part3concurrency

import zio._

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
    } yield ()

  override def run: ZIO[Any, Any, Any] = properFetchUrl
}
