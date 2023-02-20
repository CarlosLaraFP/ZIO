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
      // acquire + effect + release
      semaphore.withPermit {
        for { // critical section start
          _ <- ZIO.succeed(s"[Task $id] logged in, working...").debugThread
          result <- doWorkWhileLoggedIn
          _ <- ZIO.succeed(s"[Task $id] done: $result").debugThread
        } yield result
      }

  def demoSemaphore: UIO[Unit] =
    for {
      semaphore <- Semaphore.make(2) // Semaphore.make(1) == Mutex
      fiber1 <- login(1, semaphore).fork
      fiber2 <- login(2, semaphore).fork
      fiber3 <- login(3, semaphore).fork
      _ <- fiber1.join
      _ <- fiber2.join
      _ <- fiber3.join
    } yield ()

  def loginWeighted(permits: Int, semaphore: Semaphore): UIO[Int] =
    ZIO.succeed(s"[Task $permits] waiting to log in with $permits permits...").debugThread *>
      // acquire + effect + release
      semaphore.withPermits(permits) {
        for { // critical section starts when you acquired ALL permits
          _ <- ZIO.succeed(s"[Task $permits] logged in, working...").debugThread
          result <- doWorkWhileLoggedIn
          _ <- ZIO.succeed(s"[Task $permits] done: $result").debugThread
        } yield result
      }

  def demoSemaphoreWeighted: UIO[Unit] =
    for {
      semaphore <- Semaphore.make(2) // Semaphore.make(1) == Mutex
      fiber1 <- loginWeighted(1, semaphore).fork
      fiber2 <- loginWeighted(2, semaphore).fork
      fiber3 <- loginWeighted(3, semaphore).fork // blocks indefinitely because 3 > 2
      _ <- fiber1.join
      _ <- fiber2.join
      _ <- fiber3.join
    } yield ()

  /*
    TODO: Exercise
      - what is the code supposed to do?
      - find if there is anything wrong
      - fix the problem
  */
  val mySemaphore: UIO[Semaphore] = Semaphore.make(1) // Mutex

  val tasks: UIO[List[Int]] = Semaphore.make(1).flatMap { semaphore =>
    ZIO.collectAllPar((1 to 10).toList.map { id =>
      for {
        _ <- ZIO.succeed(s"[Task $id] waiting to log in...").debugThread
        result <- semaphore.withPermit {
          for {
            _ <- ZIO.succeed(s"[Task $id] logged in, working...").debugThread
            result <- doWorkWhileLoggedIn
            _ <- ZIO.succeed(s"[Task $id] done: $result").debugThread
          } yield result
        }
      } yield result
    })
  }

  override def run = tasks
}
