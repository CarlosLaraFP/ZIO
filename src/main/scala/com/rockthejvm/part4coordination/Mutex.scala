package com.rockthejvm.part4coordination

import zio._
import com.rockthejvm.utils._

abstract class Mutex {
  def acquire: UIO[Unit]
  def release: UIO[Unit]
}

object Mutex extends ZIOAppDefault {

  // TODO:
  //  Mutex is a concurrency primitive that allows locking an area
  //  of code from concurrent access from multiple threads (ZIO fibers)

  override def run = ???
}
