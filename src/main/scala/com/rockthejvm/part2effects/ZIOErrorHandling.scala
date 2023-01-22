package com.rockthejvm.part2effects

import zio._

object ZIOErrorHandling extends ZIOAppDefault {

  // ZIOs can fail, and this fact is embedded in the type signature of a particular ZIO

  // the Failure type can be anything that means an error to us
  val aFailedZIO: IO[String, Int] = ZIO.fail("Something went wrong")

  // RuntimeException is not thrown but rather stored in the ZIO error channel
  val failedWithThrowable: Task[Int] = ZIO.fail(new RuntimeException("Fiber (thread) crashed"))

  val failedWithDescription: IO[String, Int] = failedWithThrowable.mapError(_.getMessage)

  // attempt: run an effect that might throw an exception
  val badZIO = ZIO.succeed {
    println("Running...")
    val string: String = null
    string.length
  } // this is bad because when using ZIO.succeed we must be certain that the code inside cannot fail

  val anAttempt: Task[Int] = ZIO.attempt {
    println("Running...")
    val string: String = null
    string.length
  }

  // effectfully catch errors (end result is an effect as well)
  val catchError: UIO[Any] = anAttempt.catchAll(e => ZIO.succeed(s"Returning a different value because $e")) // catchAll eliminates error channel

  // catchSome keeps Throwable error channel because the partial function cannot guarantee at compile time that we will be able to treat all error cases
  // catchSome can also broaden the error type in the resulting expression because branches can fail with different error channel than Throwable (final: lowest common ancestor)
  val catchSelective: IO[Serializable, Any] = anAttempt.catchSome {
    case e: RuntimeException => ZIO.succeed(s"Ignoring runtime exception: $e")
    case _ => ZIO.fail("Ignoring everything else") // Failure[String] vs anAttempt's Failure[Throwable]
  }


  override def run: ZIO[Any, Any, Any] = for {
    effectA <- aFailedZIO
    effectB <- failedWithDescription
  } yield effectB
}
