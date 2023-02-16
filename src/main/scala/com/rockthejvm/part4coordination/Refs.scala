package com.rockthejvm.part4coordination

import zio._

import com.rockthejvm.utils._


object Refs extends ZIOAppDefault {

  /*
    TODO: Ref is a functionally pure atomic reference
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

  // TODO: Use cases
  //   -


  override def run: ZIO[Any, Any, Any] = ???
}
