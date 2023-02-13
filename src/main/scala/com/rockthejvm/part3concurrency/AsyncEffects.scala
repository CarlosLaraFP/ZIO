package com.rockthejvm.part3concurrency

import zio.*
import com.rockthejvm.utils.*

import java.util.concurrent.{ExecutorService, Executors}
import scala.concurrent.{ExecutionContext, ExecutionContextExecutorService, Future}

object AsyncEffects extends ZIOAppDefault {

  // CALLBACK-based

  object LoginService {
    case class AuthError(message: String)
    case class UserProfile(email: String, name: String)

    // thread pool
    val executor: ExecutorService =
      Executors.newFixedThreadPool(8)

    // "database"
    val passw = Map(
      "daniel@rockthejvm.com" -> "RockTheJVM1!"
    )

    // profile data
    val database = Map(
      "daniel@rockthejvm.com" -> "Daniel"
    )

    // Classic example of a callback-based API that executes it on some thread == Asynchronous
    def login(email: String, password: String)(onSuccess: UserProfile => Unit, onFailure: AuthError => Unit): Unit =
      executor.execute { () =>
        println(s"[${Thread.currentThread().getName}] Attempting login for $email")
        passw.get(email) match {
          //case Some(p) if p == password
          case Some(`password`) =>
            onSuccess(UserProfile(email, database(email)))
          case Some(_) =>
            onFailure(AuthError("Incorrect password."))
          case None =>
            onFailure(AuthError(s"User $email does not exist. Please sign up."))
        }
      }
  }

  import LoginService._

  // creates a ZIO that will be evaluated on some Fiber
  def loginAsZIO(id: String, pw: String): IO[AuthError, UserProfile] =
    ZIO.async[Any, AuthError, UserProfile] { cb => // callback object created by ZIO
      login(id, pw)(
        profile => cb(ZIO.succeed(profile)), // notifying the Fiber to complete the ZIO with a success
        error => cb(ZIO.fail(error)) // same with a failure
      )
    }

  val loginProgram: ZIO[Any, Object, Unit] = for {
    email <- Console.readLine("Email: ")
    password <- Console.readLine("Password: ")
    profile <- loginAsZIO(email, password).debugThread
    _ <- Console.printLine(s"Welcome to ZIO, ${profile.name}")
  } yield ()

  // TODO 1: lift a computation running on some external thread to a ZIO
  //  - hint: invoke the callback cb when the computation is complete
  //  - hint: don't wrap the computation into a ZIO
  def externalToZIO[A](computation: () => A)(executor: ExecutorService): Task[A] = ???

  val demoFirst: Task[Unit] = {
    val executor = Executors.newFixedThreadPool(8)
    val zio: Task[Int] = externalToZIO { () =>
      println(s"[${Thread.currentThread().getName}] computing on some thread...")
      Thread.sleep(1000)
      42
    } {
      executor
    }
    zio.debugThread.unit
  }

  // TODO 2: lift a Future to a ZIO
  //   - hint: invoke callback cb when the Future completes
  def futureToZIO[A](future: => Future[A])(implicit ec: ExecutionContext): Task[A] = ???
    // passing Future by-name because we don't want it executing when we pass it as an argument

  val demoSecond: Task[Unit] = {
    implicit val ec: ExecutionContextExecutorService =
      ExecutionContext.fromExecutorService(
        Executors.newFixedThreadPool(8)
      )
    val mol: Task[Int] = futureToZIO(
      Future {
        println(s"[${Thread.currentThread().getName}]  computing on some thread...")
        Thread.sleep(1000)
        42
      }
    )
    mol.debugThread.unit
  }

  // TODO 3: implement a forever ZIO
  //   - ZIO.async Fiber is semantically blocked until you invoke the callback cb
  def foreverZIO[A]: UIO[A] = ???

  override def run: ZIO[Any, Any, Any] = demoFirst
}
