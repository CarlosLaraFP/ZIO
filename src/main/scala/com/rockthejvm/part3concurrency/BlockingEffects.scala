package com.rockthejvm.part3concurrency

import zio.*
import com.rockthejvm.utils.*

import java.util.concurrent.atomic.AtomicBoolean

object BlockingEffects extends ZIOAppDefault{

  def blockingTask(n: Int): UIO[Unit] =
    ZIO.succeed(s"Running a blocking task $n").debugThread *>
      ZIO.succeed(Thread.sleep(10.seconds)) *>
        blockingTask(n)

  val program: ZIO[Any, Nothing, List[Unit]] =
    ZIO.foreachPar((1 to 100).toList)(blockingTask)
  // thread starvation due to blocking tasks

  // a separate, dedicated blocking thread pool dedicated for these tasks to avoid blocking others
  val blockingZIO: Task[Int] =
    ZIO.attemptBlocking {
      println(s"[${Thread.currentThread().getName}] running a long computation...")
      Thread.sleep(10.seconds)
      42
    }

  // blocking code cannot usually be interrupted
  val tryInterrupt: ZIO[Any, Throwable, RuntimeFlags] = for {
    blockingFib <- blockingZIO.fork
    _ <- ZIO.sleep(1.second) *> ZIO.succeed("Interrupting...").debugThread *> blockingFib.interrupt
    mol <- blockingFib.join
  } yield mol

  // based on native Thread.interrupt -> InterruptedException
  val blockingInterruptibleEffect: Task[Int] =
    ZIO.attemptBlockingInterrupt {
      println(s"[${Thread.currentThread().getName}] running a long computation...")
      Thread.sleep(10.seconds)
      42
    }

  // set a flag/switch; wrap in an atomic boolean (thread-safe variable)
  def interruptibleBlockingEffect(cancelledFlag: AtomicBoolean): Task[Unit] =
    ZIO.attemptBlockingCancelable(
      (1 to 100000).foreach { element =>
        if (!cancelledFlag.get) {
          println(element)
          Thread.sleep(100)
        }
      }
    )(ZIO.succeed(cancelledFlag.set(true)))

  val interruptibleBlocking: Task[Unit] = for {
    fib <- interruptibleBlockingEffect(new AtomicBoolean(false)).fork
    _ <- ZIO.sleep(2.seconds) *> ZIO.succeed("Interrupting...").debugThread *> fib.interrupt
    _ <- fib.join
  } yield ()

  // semantic blocking does not block threads (async); de-scheduling the effect/fiber
  // these two are not the same
  val sleeping = ZIO.sleep(1.second) // SEMANTICALLY blocking (async) and interruptible
  val sleepingThread = ZIO.succeed(Thread.sleep(1.second)) // blocking and uninterruptible
  // yield (signal/hint to the ZIO runtime to de-schedule a Fiber)
  val chainedEffect: UIO[Int] =
    (1 to 1000)
      .map(i => ZIO.succeed(i))
      .reduce(_.debugThread *> _.debugThread)
  // all evaluated in the same thread

  val yieldingDemo: UIO[Int] =
    (1 to 1000)
      .map(i => ZIO.succeed(i))
      .reduce(_.debugThread *> ZIO.yieldNow *> _.debugThread)
  // the ZIO runtime is smart enough to know that it's more efficient to run multiple
  // effects (fibers) on the same JVM thread

  override def run: ZIO[Any, Any, Any] = yieldingDemo
}
