package com.rockthejvm.part4coordination

import zio._
import zio.stm._
import com.rockthejvm.utils._

object TransactionalEffects extends ZIOAppDefault {

  /*
    TODO: Software Transactional Memory (STM)
      - Data structures to combine atomic operations ("atomic effects")
      - compose STMs to obtain other STMs (map, flatMap, etc.)
      - evaluation is fully atomic => "commit"
  */
  val anSTM: ZSTM[Any, Nothing, Int] = STM.succeed(42)
  val failedSTM: STM[String, Int] = STM.fail("Something bad")
  val attemptSTM: ZSTM[Any, Throwable, Int] = STM.attempt(42 / 0)

  // type aliases
  val ustm: USTM[Int] = STM.succeed(2)
  // in practice, STMs rarely require dependencies themselves, so USTM and STM are most common
  val anotherSTM: STM[Nothing, Int] = STM.succeed(42)

  // STM vs ZIO
  val atomicEffect: Task[Int] = attemptSTM.commit

  def transferMoney(sender: Ref[Long], receiver: Ref[Long], amount: Long): IO[String, Long] =
    for {
      senderBalance <- sender.get
      _ <- if (senderBalance < amount) ZIO.fail("Transfer failed: Insufficient funds.") else ZIO.unit
      _ <- sender.update(_ - amount)
      _ <- receiver.update(_ + amount)
      newBalance <- sender.get
    } yield newBalance

  def exploitBuggyBank =
    for {
      sender <- Ref.make(1000L)
      receiver <- Ref.make(0L)
      // One can sneak past its if condition if the other does not sender.update fast enough
      fiber1 <- transferMoney(sender, receiver, 1000).fork
      fiber2 <- transferMoney(sender, receiver, 1000).fork
      _ <- (fiber1 zip fiber2).join
      _ <- receiver.get.debugThread // should NEVER be > 1000
    } yield ()

  def loop(effect: IO[String, Unit], i: Int): IO[String, Unit] =
    if (i > 10000) ZIO.unit
    else effect.ignore *> loop(effect, i + 1)

  // TODO: STM implementation
  def transferMoneyTransactional(sender: TRef[Long], receiver: TRef[Long], amount: Long): STM[String, Long] =
    for {
      senderBalance <- sender.get
      _ <- if (senderBalance < amount) STM.fail("Transfer failed: Insufficient funds.") else STM.unit
      _ <- sender.update(_ - amount)
      _ <- receiver.update(_ + amount)
      newBalance <- sender.get
    } yield newBalance

  def cannotExploitBuggyBank: IO[String, Unit] =
    for {
      sender <- TRef.make(1000L).commit // USTM[TRef[Long]]
      receiver <- TRef.make(0L).commit
      fiber1 <- transferMoneyTransactional(sender, receiver, 1000).commit.fork
      fiber2 <- transferMoneyTransactional(sender, receiver, 1000).commit.fork
      _ <- (fiber1 zip fiber2).join
      _ <- receiver.get.commit.debugThread // should NEVER be > 1000
    } yield ()

  /*
    TODO: STM Data Structures
  */
  // atomic variable: TRef (get, update, modify, set)
  val aVariable: USTM[TRef[Int]] = TRef.make(42)

  // TODO: TArray
  val specifiedValuesTArray: USTM[TArray[Int]] = TArray.make(1, 2, 3)
  val iterableArray: USTM[TArray[Int]] = TArray.fromIterable(List(1, 2, 3, 4))

  // get/apply
  val tArrayGetElement: USTM[Int] =
    for {
      tArray <- iterableArray
      element <- tArray(2)
    } yield element

  // update
  val tArrayUpdate: USTM[TArray[Int]] =
    for {
      tArray <- iterableArray
      _ <- tArray.update(1, e => e + 10)
    } yield tArray

  // transform
  val transformedArray: USTM[TArray[Int]] =
    for {
      tArray <- iterableArray
      _ <- tArray.transform(_ * 10) // like .map, but in-place
    } yield tArray

  // fold/foldSTM, foreach

  // TODO: TSet
  val specificValuesTSet: USTM[TSet[Int]] = TSet.make(1, 2, 3, 4, 1, 2)

  val tSetContains: USTM[Boolean] =
    for {
      tSet <- specificValuesTSet
      result <- tSet.contains(3)
    } yield result

  val putElement: USTM[TSet[Int]] =
    for {
      tSet <- specificValuesTSet
      _ <- tSet.put(7) // modified in-place
    } yield tSet

  val deleteElement: USTM[TSet[Int]] =
    for {
      tSet <- specificValuesTSet
      _ <- tSet.delete(1) // modified in-place
    } yield tSet

  // union, intersect, diff, removeIf, retainIf, transform, fold, + STM versions, ...

  // TODO: TMap
  val aTMapEffect: USTM[TMap[String, Int]] =
    TMap.make(("Charles" -> 123), ("Alice" -> 369))

  val putElementTMap: USTM[TMap[String, Int]] =
    for {
      tMap <- aTMapEffect
      _ <- tMap.put("Bob", 999)
    } yield tMap

  val getElementTMap: USTM[Option[Int]] =
    for {
      tMap <- aTMapEffect
      element <- tMap.get("Alice")
    } yield element

  // delete, removeIf, retainIf, transform, foreach, fold, + STM versions
  // keys (set), values (iterable)

  // TODO: TQueue
  //  - bounded, with back pressure to block calling fibers until the queue has enough slots
  //  - unbounded, sliding, dropping
  val tQueueBounded: USTM[TQueue[Int]] = TQueue.bounded[Int](5)

  //offer/offerAll (push API)
  val demoOffer: USTM[TQueue[Int]] =
    for {
      tQueue <- tQueueBounded
      _ <- tQueue.offerAll(List(1, 3, 5, 7, 9)) // modified in-place
    } yield tQueue

  // take/takeAll/takeOption/peek (pull API) - Chunk is a ZIO wrapper over a mutable native JVM array (by-passes Scala)
  val demoTakeAll: USTM[Chunk[Int]] =
    for {
      tQueue <- demoOffer
      elements <- tQueue.takeAll
    } yield elements

  // toList, toVector, size

  // TPriorityQueue
  val maxQueue: USTM[TPriorityQueue[Int]] =
    TPriorityQueue.make(3, 4, 2, 1, 6, 5)

  /*
    TODO: Concurrent coordination
      - TRef, TPromise
  */
  val tPromiseEffect: USTM[TPromise[String, Int]] = TPromise.make[String, Int]

  // await, succeed, fail, complete

  val tPromiseAwait: STM[String, Int] =
    for {
      p <- tPromiseEffect
      result <- p.await
    } yield result

  val demoSucceed: USTM[Unit] =
    for {
      p <- tPromiseEffect
      _ <- p.succeed(100)
    } yield ()

  // TSemaphore
  val tSemaphoreEffect: USTM[TSemaphore] = TSemaphore.make(10)

  // acquire/acquireN, release/releaseN

  val semaphoreAcquire: USTM[Unit] =
    for {
      s <- tSemaphoreEffect
      _ <- s.acquire
    } yield ()

  val semaphoreRelease: USTM[Unit] =
    for {
      s <- tSemaphoreEffect
      _ <- s.release
    } yield ()

  // withPermit(s)
  val semaphoreWithPermit: UIO[Int] =
    tSemaphoreEffect.commit.flatMap { s =>
      s.withPermit {
        ZIO.succeed(42)
      }
    }

  /*
    TODO: Readers-Writers problem in computer science / distributed systems
      - TReentrantLock - can acquire the same lock multiple times without deadlocking
      - has 2 locks: read locks (lower priority) and write locks (higher priority)
  */
  val reentrantLockEffect: USTM[TReentrantLock] = TReentrantLock.make

  val demoReentrantLock: USTM[Unit] =
    for {
      lock <- reentrantLockEffect
      _ <- lock.acquireRead // acquires the read lock (writers unable to write into the shared resource)
      _ <- STM.succeed(100) // critical section (only those tht acquire the read lock can access)
      readLocked <- lock.readLocked // status of the lock, whether it's read-locked (true in this case)
      writeLocked <- lock.writeLocked // same for writer
    } yield ()

  def demoReadersWriters: UIO[Unit] = {
    def read(i: Int, lock: TReentrantLock): UIO[Unit] =
      for {
        _ <- lock.acquireRead.commit
        // critical region start
        _ <- ZIO.succeed(s"Task $i taken the read lock, reading...").debugThread
        time <- Random.nextIntBounded(1000)
        _ <- ZIO.sleep(time.millis)
        result <- Random.nextIntBounded(100) // actual computation
        _ <- ZIO.succeed(s"Task $i read value: $result").debugThread
        // critical region end
        _ <- lock.releaseRead.commit
      } yield ()

    def write(lock: TReentrantLock): UIO[Unit] =
      for {
        _ <- ZIO.sleep(200.millis)
        _ <- ZIO.succeed("Writer trying to write...").debugThread
        _ <- lock.acquireWrite.commit
        // critical region start
        _ <- ZIO.succeed("Writer: I am able to write").debugThread
        _ <- lock.releaseWrite.commit
        // critical region end
      } yield ()

    for {
      lock <- TReentrantLock.make.commit
      readersFiber <- ZIO.collectAllParDiscard((1 to 10).map(read(_, lock))).fork
      writerFiber <- write(lock).fork
      _ <- readersFiber.join
      _ <- writerFiber.join
    } yield ()
  }


  override def run = demoReadersWriters
}
