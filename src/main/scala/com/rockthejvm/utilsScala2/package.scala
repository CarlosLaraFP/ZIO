package com.rockthejvm

import zio.ZIO

package object utilsScala2 {

  implicit class DebugWrapper[R, E, A](zio: ZIO[R, E, A]) {
    def debugThread: ZIO[R, E, A] =
      zio
        .tap(value => ZIO.succeed(println(s"[${Thread.currentThread.getName}] $value")))
        .tapErrorCause(cause => ZIO.succeed(println(s"[${Thread.currentThread.getName}][FAIL] $cause")))
  }
}
