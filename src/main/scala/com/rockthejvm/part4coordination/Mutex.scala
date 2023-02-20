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
  def acquire: UIO[Unit]
  def release: UIO[Unit]
}
object Mutex {

  type Signal = Promise[Nothing, Unit]
  case class State(locked: Boolean, waiting: Queue[Signal])
  // initial state for the Ref (as always needed)
  private val unlocked = State(false, Queue.empty[Signal])

  def make: UIO[Mutex] = Ref.make(unlocked).map(createInterruptibleMutex)

  def createInterruptibleMutex(state: Ref[State]): Mutex =
    new Mutex {
      override def acquire: UIO[Unit] = ZIO.uninterruptibleMask { restore =>

        Promise.make[Nothing, Unit].flatMap { signal =>

          val cleanup: UIO[Unit] = state.modify {
            case State(flag, queue) =>
              val newQueue = queue.filterNot(_ eq signal)
              // blocked only if newQueue != queue => release the mutex
              val wasBlocked = newQueue != queue
              val decision = if (wasBlocked) ZIO.unit else release
              decision -> State(flag, newQueue)
          }.flatten

          state.modify {
            case State(false, queue) => ZIO.unit -> State(true, queue)
            case State(true, queue) =>
              restore(signal.await).onInterrupt(cleanup) -> State(true, queue.enqueue(signal))
          }.flatten
        }
      }

        override def release: UIO[Unit] =
          state.modify {
            case State(false, _) => ZIO.unit -> unlocked
            case State(true, queue) =>
              if (queue.isEmpty) ZIO.unit -> unlocked
              else {
                val (signal, newQueue) = queue.dequeue
                signal.succeed(()).unit -> State(true, newQueue)
              }
          }.flatten
      }

  def createSimpleMutex(state: Ref[State]): Mutex =
    new Mutex {
      /*
        TODO: Change the State of the Ref
          - if the Mutex is unlocked, lock it
          - if the Mutex is locked, create a Promise and add it to the Queue
            => State(true, queue + new signal) and wait on that signal
          - note: if acquire is called from a fiber while the mutex is locked, then that fiber
                  is semantically blocked until some other fiber calls release
      */
      override def acquire: UIO[Unit] =
        Promise.make[Nothing, Unit].flatMap { signal =>
          // modify partial function allows us to perform 2 things simultaneously: update a Ref and return something else per case
          state.modify { // arrow syntax for tupling
            case State(false, queue) => ZIO.unit -> State(true, queue) // JVM garbage collects signal
            case State(true, queue) => signal.await -> State(true, queue.enqueue(signal))
          }.flatten
        }
      /*
        TODO: Change the State of the Ref
          - if the mutex is unlocked, leave the State unchanged
          - if the mutex is locked
            - if the queue is empty, unlock the mutex
            - if the queue is nonempty, take the signal out of the queue and complete it
      */
      override def release: UIO[Unit] =
        state.modify {
          case State(false, _) => ZIO.unit -> unlocked
          case State(true, queue) =>
            if (queue.isEmpty) ZIO.unit -> unlocked
            else {
              val (signal, newQueue) = queue.dequeue
              signal.succeed(()).unit -> State(true, newQueue)
            }
        }.flatten
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

  def createTask(id: Int, mutex: Mutex): UIO[Int] = {
    val task = for {
      _ <- ZIO.succeed(s"[Fiber $id] attempting to acquire lock...").debugThread
      _ <- mutex.acquire // promise.await
      // critical region start
      _ <- ZIO.succeed(s"[Fiber $id] mutex acquired, working...").debugThread
      result <- workInCriticalRegion.onInterrupt(mutex.release)
      _ <- ZIO.succeed(s"[Fiber $id] complete: $result -> releasing mutex").debugThread
      // critical region end
      _ <- mutex.release // promise.succeed
      _ <- ZIO.succeed(s"[Fiber $id] released lock").debugThread
    } yield result

    task
      .onInterrupt(ZIO.succeed(s"Task $id was interrupted").debugThread)
      .onError(cause => ZIO.succeed(s"Task $id ended in error: $cause"))
  }

  def demoLockingTasks: UIO[Unit] =
    for {
      mutex <- Mutex.make
      _ <- ZIO.collectAllParDiscard(
        (1 to 10).toList.map { i =>
          createTask(i, mutex)
        })
    } yield ()

  def createInterruptingTask(id: Int, mutex: Mutex): UIO[Int] =
    if (id % 2 == 0) createTask(id, mutex)
    else for {
      fiber <- createTask(id, mutex).fork
      _ <- ZIO.sleep(2500.millis) *>
             ZIO.succeed(s"Interrupting task $id").debugThread *>
               fiber.interrupt
      result <- fiber.join
    } yield result

  /*
    After 2.5 seconds => all odd tasks will be interrupted
  */
  def demoInterruptingTasks: UIO[Unit] =
    for {
      mutex <- Mutex.make
      fiber1 <- createInterruptingTask(1, mutex).fork
      fiber2 <- createInterruptingTask(2, mutex).fork
      fiber3 <- createInterruptingTask(3, mutex).fork
      fiber4 <- createInterruptingTask(4, mutex).fork
      fiber5 <- createInterruptingTask(5, mutex).fork
      fiber6 <- createInterruptingTask(6, mutex).fork
      fiber7 <- createInterruptingTask(7, mutex).fork
      fiber8 <- createInterruptingTask(8, mutex).fork
      fiber9 <- createInterruptingTask(9, mutex).fork
      fiber10 <- createInterruptingTask(10, mutex).fork
      _ <- fiber1.await
      _ <- fiber2.await
      _ <- fiber3.await
      _ <- fiber4.await
      _ <- fiber5.await
      _ <- fiber6.await
      _ <- fiber7.await
      _ <- fiber8.await
      _ <- fiber9.await
      _ <- fiber10.await
    } yield ()


  override def run = demoInterruptingTasks
}
