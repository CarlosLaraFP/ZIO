package com.rockthejvm.part3concurrency

import zio.*
import com.rockthejvm.utils.*

import java.util.concurrent.{ExecutorService, Executors}
import scala.concurrent.{ExecutionContext, ExecutionContextExecutorService, Future}
import scala.util.{Failure, Success}

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
  def externalToZIO[A](computation: () => A)(executor: ExecutorService): Task[A] =
    ZIO.async[Any, Throwable, A] { cb =>
      executor.execute { () =>
        try {
          val result = computation()
          cb(ZIO.succeed(result))
        }
        catch {
          case e: Throwable => cb(ZIO.fail(e))
        }
      }
    }

  val demoFirst: Task[Unit] =
    externalToZIO { () =>
      println(s"[${Thread.currentThread().getName}] Computing 1st on native JVM Thread...")
      Thread.sleep(1000)
      s"Returning 1st on ZIO Fiber: 42"
    } {
      Executors.newFixedThreadPool(8)
    }
      .debugThread
      .unit

  // TODO 2: lift a Future to a ZIO
  def futureToZIO[A](future: => Future[A])(implicit ec: ExecutionContext): Task[A] =
  // passing Future by-name because we don't want it executing when we pass it as an argument
    ZIO.async[Any, Throwable, A] { cb =>
      future.onComplete {
        case Success(value) => cb(ZIO.succeed(value))
        case Failure(exception) => cb(ZIO.fail(exception))
      }
    }

  val demoSecond: Task[Unit] = {
    implicit val ec: ExecutionContextExecutorService =
      ExecutionContext.fromExecutorService(
        Executors.newFixedThreadPool(8)
      )
    val mol: Task[String] = futureToZIO(
      Future {
        println(s"[${Thread.currentThread().getName}] Computing 2nd on native JVM Thread...")
        Thread.sleep(1000)
        s"Returning 2nd on ZIO Fiber: 42"
      }
    )
    mol.debugThread.unit
  }

  // TODO 3: implement a forever ZIO
  //   - ZIO.async Fiber is semantically blocked until you invoke the callback cb
  def foreverZIO[A]: UIO[A] = ZIO.async[Any, Nothing, A] { _ => println("Forever") }


  override def run: ZIO[Any, Any, Any] = demoFirst *> demoSecond *> foreverZIO.debugThread
}