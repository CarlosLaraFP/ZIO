package com.rockthejvm.part2effects

import zio.*

import java.io.IOException
import java.net.NoRouteToHostException
import scala.util.Try

object ZIOErrorHandling extends ZIOAppDefault {

  // ZIOs can fail, and this fact is embedded in the type signature of a particular ZIO

  // the Failure type can be anything that means an error to us
  val aFailedZIO: IO[String, Int] = ZIO.fail("Something went wrong")

  // RuntimeException is not thrown but rather stored in the ZIO error channel
  val failedWithThrowable: Task[Int] = ZIO.fail(new RuntimeException("Fiber (thread) crashed"))

  val failedWithDescription: IO[String, Int] = failedWithThrowable.mapError(_.getMessage)

  // attempt: run an effect that might throw an exception
  val badZIO: UIO[Int] = ZIO.succeed {
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
  // UIO[Any] because Any is the lowest common ancestor of Int and String

  // catchSome keeps Throwable error channel because the partial function cannot guarantee at compile time that we will be able to treat all error cases
  // catchSome can also broaden the error type in the resulting expression because branches can fail with different error channel than Throwable (final: lowest common ancestor)
  val catchSelective: IO[Serializable, Any] = anAttempt.catchSome {
    case e: RuntimeException => ZIO.succeed(s"Ignoring runtime exception: $e")
    case _ => ZIO.fail("Ignoring everything else") // Failure[String] vs anAttempt's Failure[Throwable]
  }

  // chain effects
  val aBetterAttempt: UIO[Int] = anAttempt.orElse(ZIO.succeed(56))

  // fold: handle both success and failure
  val handleBoth: URIO[Any, String] = anAttempt.fold(
    e => s"Something bad happened: $e",
    v => s"The length  of the  string  is: $v"
  )

  // effectful fold: foldZIO
  val handleBothFold: UIO[String] = anAttempt.foldZIO(
    e => ZIO.succeed(s"Something bad happened: $e"),
    v => ZIO.succeed(s"The length  of the  string  is: $v")
  )

  // Conversions between Option/Try/Either to ZIO

  // Try swallows exceptions and wraps them in a Failure
  val tryToZIO: Task[Int] = ZIO.fromTry(Try(42 / 0)) // can fail with Throwable

  val anEither: Either[Int, String] = Right("Success!")

  val eitherToZIO: IO[Int, String] = ZIO.fromEither(anEither)

  // ZIO -> ZIO with Either as the value channel
  val eitherZIO: URIO[Any, Either[Throwable, Int]] = anAttempt.either
  // cannot fail, but the Throwable will be stored in the natural error channel of an Either (Left) and the value in the Right

  // reverse
  val attemptEither: Task[Int] = eitherZIO.absolve

  // option -> ZIO
  val anOption: IO[Option[Nothing], Int] = ZIO.fromOption(Some(42))

  // TODO: Implement a version of fromTry, fromOption, fromEither, either, or absolve using fold and foldZIO
  def fromEither[E, A](either: Either[E, A]): IO[E, A] = either match {
    case Left(e) => ZIO.fail(e)
    case Right(v) => ZIO.succeed(v)
  }

  val successEitherToZIO: IO[Int, String] = fromEither(Right("Test passed!"))
  val failEitherToZIO: IO[Int, String] = fromEither(Left(500))

  /*
  ` Errors: Failures present in the type signature of a ZIO
    Defects: Not present in the type signature of a ZIO & unrecoverable/unforeseen (i.e. the bad ZIO above)

    ZIO[R, E, A] can finish with Exit[E, A]:
     - Success[A] containing A
     - Cause[E]:
      - Fail[E] containing the error ("clean" failure)
      - Die(t: Throwable) which was unforeseen
  */

  val divisionByZero: UIO[Int] = ZIO.succeed(1 / 0)

  val failedInt: ZIO[Any, String, Int] = ZIO.fail("I failed")
  val failureCauseExposed: ZIO[Any, Cause[String], Int] = failedInt.sandbox
  val failureCauseHidden: ZIO[Any, String, Int] = failureCauseExposed.unsandbox
  // fold with cause
  val foldedWithCause: URIO[Any, String] = failedInt.foldCause(
    cause => s"This failed with ${cause.defects}",
    value => s"This succeeded with $value"
  )
  val foldedWithCauseZIO: ZIO[Any, Nothing, String] = failedInt.foldCauseZIO(
    cause => ZIO.succeed(s"This failed with ${cause.defects}"),
    value => ZIO.succeed(s"This succeeded with $value")
  )

  /*
    TODO: Good practice
      - at a lower level, your "errors" should be treated
      - at a higher level, you should hide "errors" and assume they are unrecoverable
        (make effects as infallible as possible and surface causes)
  */

  // How to turn an error into a defect?
  def callHTTPEndpoint(url: String): ZIO[Any, IOException, String] =
    ZIO.fail(new IOException("No  internet!"))

  val endpointCallWithDefects: UIO[String] =
    callHTTPEndpoint("rockthejvm.com").orDie // all errors are now defects (exception swallowed)

  // refining the error channel
  def callHTTPEndpointWideError(url: String): ZIO[Any, Exception, String] =
    ZIO.fail(new IOException("No  internet wide!"))

  // partial function (does not cover entire exception spectrum)
  def callHTTPEndpointRefined(url: String): ZIO[Any, IOException, String] =
    callHTTPEndpointWideError(url).refineOrDie[IOException] {
      case e: IOException => e
      case _: NoRouteToHostException => new IOException(s"No route to  host to $url, can't fetch page.")
    }

  // reverse: turn effects into the error channel
  val endpointCallWithError: IO[String, String] = endpointCallWithDefects.unrefine {
    case e => e.getMessage
  }


  override def run: ZIO[Any, Any, Any] = {

    val composedErrorHandledEffects = for {
      effectA <- catchError
      effectB <- catchSelective
      effectC <- eitherToZIO
      effectD <- successEitherToZIO
      effectE <- failEitherToZIO.catchAll(e => ZIO.succeed(e.toString))
    } yield Vector(effectA, effectB, effectC, effectD, effectE)

    composedErrorHandledEffects.map(println)
  }
}
