package com.rockthejvm.part3concurrency

import zio._
import com.rockthejvm.utils._

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

  // zipPar, zipWithPar

  // collectAllPar
  val effects: Seq[UIO[Int]] = (1 to 10).map(i => ZIO.succeed(i).debugThread)
  val collectedValues: UIO[Seq[Int]] = ZIO.collectAllPar(effects)
  // this general concept of unwrapping two wrapper types inside out is called "Traverse"
  // collectAllPar has the property that the values contained in the Seq are in the same order
  // as the effects were created in the original sequence, even though they are evaluated in different threads

  // foreachPar
  val printlnParallel: UIO[List[Unit]] = ZIO.foreachPar((1 to 10).toList)(i => ZIO.succeed(println(i)))

  // reduceAllPar, mergeAllPar
  val sumPar: UIO[Int] = ZIO.reduceAllPar(ZIO.succeed(0), effects)(_ + _) // reduce on effects
  val sumParMerge: UIO[Int] = ZIO.mergeAllPar(effects)(0)(_ + _) // more general version of reduceAllPar

  /*
    if all the effects succeed, we are good
    if one effect fails, all others are interrupted, and the first failed effect's error is surfaced
    if one effect is interrupted, all others are also interrupted, and the error = interruption itself
    if the entire parent Fiber is interrupted, all effects are interrupted
  */

  // TODO: Count the words in all files using a parallel combinator (1+ from above)
  def countWords(filePath: String): UIO[Int] =
    ZIO.succeed {
      val source = scala.io.Source.fromFile(filePath)
      val nWords = source.getLines.mkString(" ").split(" ").count(_.nonEmpty)
      source.close
      nWords
    }

  def countWordsParallel(nFiles: Int): UIO[Int] =
    ZIO.reduceAllPar(
      ZIO.succeed(0),
      (1 to nFiles)
        .map(i =>
          countWords(
            s"src/main/resources/testfile_$i.txt"
          )
        )
    )(_ + _)


  override def run: ZIO[Any, Any, Any] = countWordsParallel(10).debugThread
}
