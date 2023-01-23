package com.rockthejvm.part2effects

import zio._

object ZIODependencies extends ZIOAppDefault {

  // app to subscribe users to newsletter
  case class User(name: String, email: String)

  class UserSubscription(emailService: EmailService, userDatabase: UserDatabase) {
    def subscribeUser(user: User): Task[Unit] =
      for {
        _ <- emailService.email(user)
        _ <- userDatabase.insert(user)
      } yield ()
  }

  object UserSubscription {
    def create(emailService: EmailService, userDatabase: UserDatabase): UserSubscription =
      new UserSubscription(emailService, userDatabase)
  }

  class EmailService {
    def email(user: User): Task[Unit] =
      ZIO.succeed(println(s"You have just been subscribed. Welcome, ${user.name}!"))
  }

  object EmailService {
    def create(): EmailService = new EmailService
  }

  class UserDatabase(connectionPool: ConnectionPool) {
    def insert(user: User): Task[Unit] =
      for {
        conn <- connectionPool.get
        _ <- conn.runQuery(s"INSERT INTO subscribers(name, email) VALUES ($user.name}, ${user.email}")
      } yield ()
  }

  object UserDatabase {
    def create(connectionPool: ConnectionPool): UserDatabase = new UserDatabase(connectionPool)
  }

  class ConnectionPool(nConnections: Int) {
    def get: Task[Connection] = ZIO.succeed(println("Acquired connection")) *> ZIO.succeed(Connection())
  }

  object ConnectionPool {
    def create(nConnections: Int) = new ConnectionPool(nConnections)
  }

  case class Connection() {
    def runQuery(query: String): Task[Unit] = ZIO.succeed(println(s"Executing query: $query"))
  }


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
  def subscribeBetter(user: User): ZIO[UserSubscription, Throwable, Unit] = for {
    sub <- ZIO.service[UserSubscription] // ZIO[UserSubscription, Nothing, UserSubscription]
    _ <- sub.subscribeUser(user)
  } yield ()

  val programBetter: ZIO[UserSubscription, Throwable, Unit] = for {
    _ <- subscribeBetter(User("Alice", ""))
    _ <- subscribeBetter(User("Bob", ""))
    _ <- subscribeBetter(User("Charlie", ""))
  } yield ()

  /*
    TODO: Advantages
      - we don't need to care about dependencies until the end of the world
      - resource leaks eliminated because all ZIOs requiring a dependency will use the same instance
      - can use different instances of the same type for different needs (i.e. testing)
      - ZLayers can be created and composed much like regular ZIOs, with a very rich API
  */

  // ZLayers



  override def run: ZIO[Any, Any, Any] = programBetter.provideLayer(
    ZLayer.succeed(
      UserSubscription.create(
        EmailService.create(),
        UserDatabase.create(
          ConnectionPool.create(10)
        )
      )
    )
  )
    //subscribe(User("Daniel", "email@email.com"))
}
