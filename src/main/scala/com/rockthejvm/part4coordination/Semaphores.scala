package com.rockthejvm.part4coordination

import com.rockthejvm.utils._
import zio._


object Semaphores extends ZIOAppDefault {

  // TODO: Semaphore use case: Limit # of concurrent sessions (i.e. managing connection pool)
  // ZIO concurrency primitive => more general version of a Mutex with n permits (internal counter)
  // acquire, acquireN (can potentially block semantically)
  // release, releaseN
  val semaphore: UIO[Semaphore] = Semaphore.make(10)

  // example: limiting the number of concurrent sessions on a server
  def doWorkWhileLoggedIn: UIO[Int] =
    ZIO.sleep(1.second) *> Random.nextIntBounded(100)

  def login(id: Int, semaphore: Semaphore): UIO[Int] =
    ZIO.succeed(s"[Task $id] waiting to log in...").debugThread *>
      semaphore.withPermit { // object.synchronized { }
        for {
          // critical section start
          _ <- ZIO.succeed(s"[Task $id] logged in, working...").debugThread
          result <- doWorkWhileLoggedIn
          _ <- ZIO.succeed(s"[Task $id] done: $result").debugThread
        } yield result
      }

  def demoSemaphore: UIO[Unit] =
    for {
      semaphore <- Semaphore.make(2)
      fiber1 <- login(1, semaphore).fork
      fiber2 <- login(2, semaphore).fork
      fiber3 <- login(3, semaphore).fork
      _ <- fiber1.join
      _ <- fiber2.join
      _ <- fiber3.join
    } yield ()

  override def run = demoSemaphore
}
