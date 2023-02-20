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
      _ <- if (senderBalance < amount) ZIO.fail("Transfer failed: Insufficient funds.")
           else ZIO.unit
      _ <- sender.update(_ - amount)
      _ <- receiver.update(_ + amount)
      newBalance <- sender.get
    } yield newBalance

  def exploitBuggyBank =
    for {
      sender <- Ref.make(1000L)
      receiver <- Ref.make(0L)
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

  def cannotExploitBuggyBank =
    for {
      sender <- TRef.make(1000L).commit // USTM[TRef[Long]]
      receiver <- TRef.make(0L).commit
      fiber1 <- transferMoneyTransactional(sender, receiver, 1000).commit.fork
      fiber2 <- transferMoneyTransactional(sender, receiver, 1000).commit.fork
      _ <- (fiber1 zip fiber2).join
      _ <- receiver.get.commit.debugThread // should NEVER be > 1000
    } yield ()

  override def run = loop(cannotExploitBuggyBank, 1)
}
