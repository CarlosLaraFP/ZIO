package com.rockthejvm.part2effects

import zio._

object ZIOApps extends ZIOAppDefault {
  // TODO: ZIOAppDefault provides Runtime, Trace [across JVM threads], and everything needed to evaluate ZIO effects

  // TODO: This is the fundamental entry point of the ZIO application that runs one giant composed effect (effect chains)
  // TODO: Spinning up of the entire application infrastructure (i.e. HttpService, Database, ...)
  override def run: ZIO[Any, Any, Any] = ZIO.succeed(42).flatMap(mol => ZIO.succeed(println(mol)))

  //val meaningOfLife: UIO[Int] = ZIO.succeed(42)

  //override def run: ZIO[Any, Any, Any] = meaningOfLife.debug
}

// NOT NEEDED IN PRACTICE
object ManualApp extends ZIOApp {

  override implicit def environmentTag: zio.EnvironmentTag[ManualApp.type] = ???

  override type Environment = this.type

  override def bootstrap: ZLayer[Any, Any, ManualApp.type] = ???

  override def run: ZIO[Any, Any, Any] = ???
}
