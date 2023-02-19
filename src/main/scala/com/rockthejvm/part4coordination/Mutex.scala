package com.rockthejvm.part4coordination

import zio._
import com.rockthejvm.utils._

// TODO:
//  Mutex is a concurrency primitive that allows locking an area
//  of code from concurrent access from multiple threads (ZIO fibers)

abstract class Mutex {
  // if a fiber calls acquire, this mutex will be locked for any fibers that subsequently call acquire
  // (semantically blocked until the lock is released by the initial fiber)
  def acquire: UIO[Unit]
  def release: UIO[Unit]
}
object Mutex {
  def make: UIO[Mutex] = ZIO.succeed {
    new Mutex {
      private val ref = Ref.make[Boolean]
      private val promise = Promise.make[Throwable, Boolean]

      // we need a semantic block if another fiber is in progress
      override def acquire: UIO[Unit] =
        for {
          p <- promise
          _ <- p.await
        } yield ()

      // Ref back to false
      override def release: UIO[Unit] =
        for {
          p <- promise
          _ <- p.succeed(true)
        } yield ()
    }
  }
}

object MutexPlayground extends ZIOAppDefault {

  def workInCriticalRegion: UIO[Int] =
    ZIO.sleep(1.second) *> Random.nextIntBounded(100)

  def demoNonLockingTasks: UIO[Unit] =
    ZIO.collectAllParDiscard((1 to 10).toList.map { i =>
      for {
        _ <- ZIO.succeed(s"[Task $i] working...").debugThread
        result <- workInCriticalRegion
        _ <- ZIO.succeed(s"[Task $i] complete: $result").debugThread
      } yield ()
    })

  def createTask(id: Int, mutex: Mutex): UIO[Int] =
    for {
      _ <- ZIO.succeed(s"[Fiber $id] attempting to acquire lock...").debugThread
      _ <- mutex.acquire // promise.await
      // critical region start
      _ <- ZIO.succeed(s"[Fiber $id] mutex acquired, working...").debugThread
      result <- workInCriticalRegion
      _ <- ZIO.succeed(s"[Fiber $id] complete: $result -> releasing mutex").debugThread
      // critical region end
      _ <- mutex.release // promise.succeed
      _ <- ZIO.succeed(s"[Fiber $id] released lock").debugThread
    } yield result

  def demoLockingTasks: UIO[Unit] =
    for {
      mutex <- Mutex.make
      _ <- ZIO.collectAllParDiscard(
        (1 to 10).toList.map { i =>
          createTask(i, mutex)
        })
    } yield ()


  override def run = demoLockingTasks
}
