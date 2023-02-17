package com.rockthejvm.part4coordination

import zio._

import com.rockthejvm.utils._


object Refs extends ZIOAppDefault {

  /*
    TODO: Ref is a functionally pure atomic reference (thread-safe state management)
      - thread-safe variable which protects, read, and writes from concurrent modifications
        from multiple threads
      - atomic means that once the operation starts, all other threads that try to interact
        with the atomic reference will be blocked

    TODO: Use cases
      - concurrent and thread-safe reads or writes over some shared values (functionally pure)
  */
  val atomicMol: UIO[Ref[Int]] = Ref.make(42)

  // obtain a value
  val mol: UIO[Int] = atomicMol.flatMap { ref =>
    ref.get // thread-safe getter
  }

  // changing
  val setMol: UIO[Unit] = atomicMol.flatMap { ref =>
    ref.set(84) // thread-safe getter
  }

  // get + set in 1 atomic operation
  val getSetMol: UIO[Int] = atomicMol.flatMap { ref =>
    ref.getAndSet(500)
  }

  // update - run a function on the value contained in the reference
  val updatedMol: UIO[Unit] = atomicMol.flatMap { ref =>
    ref.update(_ * 100)
  }

  // update + get in 1 atomic operation
  val updatedMolWithValue: UIO[Int] = atomicMol.flatMap { ref =>
    ref.updateAndGet(_ * 100) // returns the new value
    ref.getAndUpdate(_ * 100) // returns the old value
  }

  // modify
  val modifiedMol: UIO[String] = atomicMol.flatMap { ref =>
    ref.modify { value =>
      (s"My current value is $value", value * 200)
    }
  }

  // example: distributing work
  // not thread-safe, hard to debug, mixing pure and impure code
  def demoConcurrentWorkImpure: UIO[Unit] = {
    var count = 0

    def task(workload: String): UIO[Unit] = {
      val wordCount = workload.split(" ").length
      for {
        _ <- ZIO.succeed(s"Counting words for: $workload: $wordCount").debugThread
        newCount <- ZIO.succeed(count + wordCount)
        _ <- ZIO.succeed(s"New total: $newCount").debugThread
        _ <- ZIO.succeed(count += wordCount) // update the variable
      } yield ()
    }

    val effects = List("ZIO is init", "Ref is latest", "Scala is foundation", "Caliban is king").map(task)

    ZIO.collectAllParDiscard(effects)
  }
  // every fiber sees its own version of the variable

  def demoConcurrentWorkPure: UIO[Unit] = {
    def task(workload: String, total: Ref[Int]): UIO[Unit] = {
      val wordCount = workload.split(" ").length
      for {
        _ <- ZIO.succeed(s"Counting words for: $workload: $wordCount").debugThread
        newCount <- total.updateAndGet(_ + wordCount)
        _ <- ZIO.succeed(s"New total: $newCount").debugThread
      } yield ()
    }

    for {
      counter <- Ref.make(0)
      _ <- ZIO.collectAllParDiscard(
        List("ZIO is init", "Ref is latest", "Scala is foundation", "Caliban is king")
          .map(load => task(load, counter))
      )
    } yield ()
  }
  // all fibers are blocked until the single fiber currently holding the lock releases it

  // the update and modify functions may be run more than once if locked out on mutation attempt
  def demoMultipleUpdates: UIO[Unit] = {
    def task(id: Int, ref: Ref[Int]): UIO[Unit] =
      ref.modify(previous => (println(s"Task $id updating Ref at $previous"), id))

    for {
      ref <- Ref.make(0)
      _ <- ZIO.collectAllParDiscard((1 to 10).toList.map(i => task(i, ref)))
    } yield ()
  }


  override def run: ZIO[Any, Any, Any] = demoMultipleUpdates
}
