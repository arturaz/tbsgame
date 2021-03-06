import sbt.Keys._

name := "tbsgame"

resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"

resolvers += Resolver.sonatypeRepo("releases")

resolvers += Resolver.sonatypeRepo("snapshots")

resolvers += "spray repo" at "http://repo.spray.io"

val commonSettings = Seq(
  scalaVersion := "2.11.7",
  scalacOptions := Seq("-feature", "-unchecked")
)

val macrosSettings = commonSettings ++ Seq(
  libraryDependencies <+= scalaVersion("org.scala-lang" % "scala-reflect" % _)
)

val akkaVersion = "2.4.0"

val scalazVersion = "7.1.4"

val rootSettings = commonSettings ++ Seq(
  version := "1.0",
  scalacOptions ++= Seq("-deprecation", "-Xlint", "-Xfatal-warnings"),
  unmanagedSourceDirectories in Compile += baseDirectory.value / "src" / "gen" / "scala",
  initialCommands in console := """import app.models._, world._, implicits._""",
  libraryDependencies ++= Seq(
    "com.typesafe.akka" %% "akka-actor" % akkaVersion,
    "com.typesafe.akka" %% "akka-typed-experimental" % akkaVersion,
    "net.databinder.dispatch" %% "dispatch-core" % "0.11.2",
    "org.scalaz" %% "scalaz-core" % scalazVersion,
    "org.scalaz" %% "scalaz-effect" % scalazVersion,
    "commons-io" % "commons-io" % "2.4",
    "com.github.t3hnar" %% "scala-bcrypt" % "2.4",
    "com.github.nscala-time" %% "nscala-time" % "1.8.0",
    "com.beachape" %% "enumeratum" % "1.0.1",
    "org.spire-math" %% "spire" % "0.9.0",
    "com.trueaccord.scalapb" %% "scalapb-runtime" % "0.4.8",
    "io.argonaut" %% "argonaut" % "6.1-M4"
  ) ++ Seq(
    "org.flywaydb" % "flyway-core" % "3.1",
    "com.zaxxer" % "HikariCP-java6" % "2.2.5",
    "org.xerial" % "sqlite-jdbc" % "3.8.7",
    "com.typesafe.slick" %% "slick" % "2.1.0"
  ) ++ {
    val monocleLibVer = "0.5.1"
    Seq(
      "com.github.julien-truffaut"  %%  "monocle-core"    % monocleLibVer,
      "com.github.julien-truffaut"  %%  "monocle-generic" % monocleLibVer,
      "com.github.julien-truffaut"  %%  "monocle-macro"   % monocleLibVer,         // since 0.4.0
      "com.github.julien-truffaut"  %%  "monocle-law"     % monocleLibVer % "test" // since 0.4.0
    )
  } ++ Seq(
    "org.scalatest" % "scalatest_2.11" % "2.2.1" % "test"
  )
)

lazy val macros = project.settings(macrosSettings:_*)

lazy val root = (project in file(".")).dependsOn(macros).settings(rootSettings:_*)
