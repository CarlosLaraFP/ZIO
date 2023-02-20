package com.rockthejvm.part4coordination

import zio._
import com.rockthejvm.utils._
import scala.collection.immutable.Queue

// TODO:
//  Mutex is a concurrency primitive that allows locking an area
//  of code from concurrent access from multiple threads (ZIO fibers)

abstract class Mutex {
  // if a fiber calls acquire, this mutex will be locked for any fibers that subsequently call acquire
  // (semantically blocked until the lock is released by the initial fiber)
  def acquire: Task[Unit]
  def release: UIO[Unit]
}
object Mutex {

  type Signal = Promise[Nothing, Unit]
  private case class State(locked: Boolean, waiting: Queue[Signal])

  private val unlocked = State(locked = false, Queue.empty[Signal])

  def make: UIO[Mutex] = Ref.make(unlocked).map { (state: Ref[State]) =>
    new Mutex {
      /*
        TODO: Change the State of the Ref
          - if the Mutex is unlocked, lock it
          - if the Mutex is locked, create a Promise and add it to the Queue
            => State(true, queue + new signal) and wait on that signal
          - note: if acquire is called from a fiber while the mutex is locked, then that fiber
                  is semantically blocked until some other fiber calls release
      */
      override def acquire: Task[Unit] = ???

      /*
        TODO: Change the State of the Ref
          - if the mutex is unlocked, leave the State unchanged
          - if the mutex is locked
            - if the queue is empty, unlock the mutex
            - if the queue is nonempty, take the signal out of the queue and complete it
      */
      override def release: UIO[Unit] = ???
    }
  }
}

object MutexPlayground extends ZIOAppDefault {

  def workInCriticalRegion: UIO[Int] =
    ZIO.sleep(3.seconds) *> Random.nextIntBounded(100)

  def demoNonLockingTasks: UIO[Unit] =
    ZIO.collectAllParDiscard((1 to 10).toList.map { i =>
      for {
        _ <- ZIO.succeed(s"[Task $i] working...").debugThread
        result <- workInCriticalRegion
        _ <- ZIO.succeed(s"[Task $i] complete: $result").debugThread
      } yield ()
    })

  def createTask(id: Int, mutex: Mutex): Task[Int] =
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

  def demoLockingTasks: Task[Unit] =
    for {
      mutex <- Mutex.make
      _ <- ZIO.collectAllParDiscard(
        (1 to 10).toList.map { i =>
          createTask(i, mutex)
        })
    } yield ()


  override def run = demoLockingTasks
}
