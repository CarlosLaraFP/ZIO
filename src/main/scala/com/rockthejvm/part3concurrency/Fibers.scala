package com.rockthejvm.part3concurrency

import zio._

object Fibers extends ZIOAppDefault {

  // Normally, these effects execute synchronously on the same thread when using combinators (i.e. for-comprehensions)
  val meaningOfLife = ZIO.succeed(42)
  val favoriteLanguage = ZIO.succeed("Scala")

  /*
    TODO: Fiber == lightweight thread on native JVM
      - description of a computation that will be performed by one of the threads managed by the ZIO runtime
      - we use ZIO APIs to start Fibers automatically and schedule them on the thread pool managed by the ZIO runtime
   */
  def createFiber: Fiber[Throwable, String] = ??? // impossible to create manually



  override def run: ZIO[Any, Any, Any] = ???
}
