package com.rockthejvm.part3concurrency

import zio._

import com.rockthejvm.utils._

object MasteringInterruptions extends ZIOAppDefault {

  /*
    TODO: Interruptions
      - fiber.interrupt
      - ZIO.race, ZIO.zipPar, ZIO.collectAllPair
      - outliving parent fiber
      - manual
  */

  val manuallyInterrupted: ZIO[Any, Any, Int] =
    ZIO.succeed("Computing...").debugThread *> ZIO.interrupt *> ZIO.succeed(42).debugThread

  val interruptedFinalizer: ZIO[Any, Any, Int] =
    manuallyInterrupted.onInterrupt(ZIO.succeed("I was interrupted").debugThread)

  // uninterruptible
  // payment flow must NOT be interrupted
  val fuzzyPaymentSystem = (
    ZIO.succeed("Payment running, don't cancel me...").debugThread *>
    ZIO.sleep(1.second) *> // simulating payment
    ZIO.succeed("Payment complete").debugThread
  ).onInterrupt( // we do NOT want this triggered
    ZIO.succeed("CANCEL!").debugThread
  )

  val cancellation = for {
    fiber <- fuzzyPaymentSystem.fork
    _ <- ZIO.sleep(500.millis) *> fiber.interrupt
    _ <- fiber.join
  } yield ()

  // ZIO.uninterruptible (atomic effect)
  val atomicPayment: Task[String] = ZIO.uninterruptible(fuzzyPaymentSystem)
  val sameAtomicPayment: Task[String] = fuzzyPaymentSystem.uninterruptible

  val betterCancellation = for {
    fiber <- atomicPayment.fork
    _ <- ZIO.sleep(500.millis) *> fiber.interrupt
    _ <- fiber.join
  } yield ()

  // interruptibility is regional
  val zioA = ZIO.succeed(1)
  val zioB = ZIO.succeed(2)
  val zioC = ZIO.succeed(3)
  // ALL effects in the composition are uninterruptible
  val zioComposed = (zioA *> zioB *> zioC).uninterruptible

  // inner scopes override outer scopes (selective interruptibility in a composed effect)
  val anotherComposed = (zioA *> zioB.interruptible *> zioC).uninterruptible

  // manual approach above is rare because there is a more powerful API: uninterruptibleMask

  /*
    TODO: Authentication Service
      - input password, can be interrupted, because otherwise is might block the fiber indefinitely
      - verify password, which cannot be interrupted once it's triggered
  */
  val inputPassword: UIO[String] = for {
    _ <- ZIO.succeed("Input password:").debugThread
    _ <- ZIO.succeed("Typing password...").debugThread
    _ <- ZIO.sleep(2.seconds)
    password <- ZIO.succeed("RockTheJVM1!")
  } yield password

  def verifyPassword(password: String): UIO[Boolean] =
    for {
      _ <- ZIO.succeed("Verifying...").debugThread
      _ <- ZIO.sleep(2.seconds)
      result <- ZIO.succeed(password == "RockTheJVM1!")
    } yield result

  // mask makes the entire wrapped effect uninterruptible, EXCEPT the ones shielded by restore
  val authFlow = ZIO.uninterruptibleMask { restore =>
    for {
      password <- restore(inputPassword).onInterrupt(
        ZIO.succeed("Authentication timed out. Try again later.").debugThread
      )
      verification <- verifyPassword(password)
      _ <- if (verification) ZIO.succeed("Authentication successful.").debugThread else ZIO.succeed("Authentication failed.")
    } yield ()
  }

  // interruption technically does happen, but after the entire effect is done
  val authProgram = for {
    authFiber <- authFlow.fork
    _ <- ZIO.sleep(3.seconds) *> ZIO.succeed("Attempting to cancel authentication...").debugThread *> authFiber.interrupt
    _ <- authFiber.join
  } yield ()

  // What will these do?

  // since the second effect is interruptible by default, ZIO.interrupt will interrupt it (and all subsequent)
  val cancelBeforeMol: UIO[Int] =
    ZIO.interrupt *> ZIO.succeed(42).debugThread

  // due to scope, ZIO.interrupt overrides and interrupts all subsequent effects
  val uncancelBeforeMol: UIO[Int] =
    ZIO.uninterruptible(ZIO.interrupt *> ZIO.succeed(42).debugThread)

  // cannot be interrupted because authFlow receives restore from uninterruptible caller
  val anotherAuthProgram: UIO[Unit] =
    for {
      authFiber <- ZIO.uninterruptibleMask(_ => authFlow).fork
      _ <- ZIO.sleep(3.seconds) *> ZIO.succeed("Attempting to cancel authentication...").debugThread *> authFiber.interrupt
      _ <- authFiber.join
    } yield ()

  val threeStepProgram: UIO[Unit] = {
    val sequence = ZIO.uninterruptibleMask { restore =>
      for {
        _ <- restore(ZIO.succeed("Interruptible").debugThread *> ZIO.sleep(1.second)) // interruptible
        _ <- ZIO.succeed("Uninterruptible").debugThread *> ZIO.sleep(1.second) // uninterruptible
        _ <- restore(ZIO.succeed("Interruptible Two").debugThread *> ZIO.sleep(1.second)) // interruptible
      } yield ()
    }
    for {
      fiber <- sequence.fork
      _ <- ZIO.sleep(1500.millis) *> ZIO.succeed("INTERRUPTING!").debugThread *> fiber.interrupt
      _ <- fiber.join
    } yield ()
  }


  override def run: ZIO[Any, Any, Any] = threeStepProgram
}
