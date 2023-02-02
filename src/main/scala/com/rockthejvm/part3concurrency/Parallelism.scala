package com.rockthejvm.part3concurrency

import zio._

object Parallelism extends ZIOAppDefault {

  // 2 parallel effects can run at the same time, while concurrent effects overlap in some way
  val meaningOfLife: ZIO[Any, Nothing, Int] = ZIO.succeed(42)
  val favoriteLanguage: ZIO[Any, Nothing, String] = ZIO.succeed("Scala")

  // combines/zips in a sequential way because zip is based on flatMap
  val combined: ZIO[Any, Nothing, (Int, String)] = meaningOfLife.zip(favoriteLanguage)

  // combination in parallel: every effect will run on its own fiber
  // and when those fibers complete, the tuple will be created by the ZIO runtime in the resulting effect
  val combinedPar: ZIO[Any, Nothing, (Int, String)] = meaningOfLife.zipPar(favoriteLanguage)

  /*
    - start each on fibers
    - what if one fails? the other one should be interrupted
    - what if one is interrupted by another fiber? the entire thing should be interrupted
    - what if the whole thing is interrupted? need to interrupt both effects
  */

  // TODO: Implement zipPar combinator (hint: fork, join, await, interrupt)
  //  - NOT CORRECT UNTIL THE END OF THE COURSE
  def parallelZip[R, E, A, B](zioA: ZIO[R, E, A], zioB: ZIO[R, E, B]): ZIO[R, E, (A, B)] = {
    val exits = for {
      fiberA <- zioA.fork
      fiberB <- zioB.fork
      exitA <- fiberA.await
      exitB <- exitA match {
        case Exit.Success(value) => fiberB.await
        case Exit.Failure(_) => fiberB.interrupt
      }
    } yield (exitA, exitB)

    exits.flatMap {
      case (Exit.Success(a), Exit.Success(b)) => ZIO.succeed((a, b)) // successful path
      case (Exit.Success(_), Exit.Failure(c)) => ZIO.failCause(c) // B failed
      case (Exit.Failure(c), Exit.Success(_)) => ZIO.failCause(c) // A failed
      case (Exit.Failure(cA), Exit.Failure(cB)) => ZIO.failCause(cA && cB)
    }
  }

  // parallel combinators


  override def run: ZIO[Any, Any, Any] = ???
}
