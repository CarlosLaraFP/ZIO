name := "zio"
version := "0.1"
scalaVersion := "3.2.1"
//scalaVersion := "3.1.3"

lazy val zioVersion = "2.0.6"
lazy val catsVersion = "2.9.0"

libraryDependencies ++= Seq(
  "dev.zio" %% "zio" % zioVersion,
  "dev.zio" %% "zio-test" % zioVersion,
  "dev.zio" %% "zio-test-sbt" % zioVersion,
  "dev.zio" %% "zio-streams" % zioVersion,
  "dev.zio" %% "zio-test-junit" % zioVersion,
  "org.typelevel" %% "cats-core" % catsVersion
  //"dev.zio" %% "zio-interop-cats" % "23.0.0.0"
)

testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework")

//scalacOptions ++= Seq("-language:higherKinds")