package com.rockthejvm.part2effects

import zio._

object ZIODependencies extends ZIOAppDefault {

  // app to subscribe users to newsletter
  import ServiceModel._


  // Dependency injection
  val subscriptionService: UIO[UserSubscription] = ZIO.succeed(
    UserSubscription.create(
      EmailService.create(),
      UserDatabase.create(
        ConnectionPool.create(10)
      )
    )
  )
  /*
    This "clean" DI has drawbacks:
      - does not scale for many services
      - can be worse if not organized properly
        - pass dependencies partially
        - not having all dependencies in the same place (for inspecting dependency graph)
        - passing dependencies multiple times can cause leaking resources
  */

  def subscribe(user: User): Task[Unit] = for {
    sub <- subscriptionService // service is instantiated at the point of call (imagine a million instances concurrently)
    _ <- sub.subscribeUser(user)
  } yield ()

  /*
    TODO:
      - risk leaking resources if you subscribe multiple users in the same program
      - oblivious to many instances running simultaneously
  */

  val program: Task[Unit] = for {
    _ <- subscribe(User("Alice", ""))
    _ <- subscribe(User("Bob", ""))
    _ <- subscribe(User("Charlie", ""))
  } yield ()

  // alternative
  def subscribeBetter(user: User): ZIO[UserSubscription, Throwable, Double] = for {
    sub <- ZIO.service[UserSubscription] // ZIO[UserSubscription, Nothing, UserSubscription]
    _ <- sub.subscribeUser(user)
  } yield 12.12

  val programBetter: ZIO[UserSubscription, Throwable, Double] = for {
    _ <- subscribeBetter(User("Alice", ""))
    _ <- subscribeBetter(User("Bob", ""))
    _ <- subscribeBetter(User("Charlie", ""))
  } yield 12.12

  /*
    TODO: Advantages
      - we don't need to care about dependencies until the end of the world
      - resource leaks eliminated because all ZIOs requiring a dependency will use the same instance
      - can use different instances of the same type for different needs (i.e. testing)
      - ZLayers can be created and composed much like regular ZIOs, with a very rich API
  */

  // ZLayers
  val connectionPoolLayer: ZLayer[Any, Nothing, ConnectionPool] = ZLayer.succeed(ConnectionPool.create(10))
  /*
    A ZLayer that requires a dependency (higher layer) can be built with ZLayer.fromFunction
    and automatically fetch the function arguments and place them into the ZLayer's dependency/environment type argument.

    Fetching is done through macros at compile time.
  */
  val databaseLayer: ZLayer[ConnectionPool, Nothing, UserDatabase] =
    ZLayer.fromFunction(UserDatabase.create _)

  val emailServiceLayer: ZLayer[Any, Nothing, EmailService] =
    ZLayer.succeed(EmailService.create())

  val userSubscriptionServiceLayer: ZLayer[UserDatabase with EmailService, Nothing, UserSubscription] =
    ZLayer.fromFunction(UserSubscription.create _)

  // composing layers

  // Vertical Composition
  val databaseLayerFull: ZLayer[Any, Nothing, UserDatabase] = connectionPoolLayer >>> databaseLayer

  // Horizontal Composition: Combines dependencies of both layers AND the values of both layers (E common ancestor)
  val subscriptionRequirementsLayer: ZLayer[Any, Nothing, UserDatabase with EmailService] =
    databaseLayerFull ++ emailServiceLayer

  // mix & match
  val userSubscriptionLayer: ZLayer[Any, Nothing, UserSubscription] =
    subscriptionRequirementsLayer >>> userSubscriptionServiceLayer

  /*
    TODO: Best practices =>
      - create layers in the companion objects of the services you want to expose
  */

  override def run: ZIO[Any, Any, Any] = programBetter.provide(userSubscriptionLayer)
}
