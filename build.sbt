import sbt.Keys._

name := "TBS Game"

resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"

resolvers += Resolver.sonatypeRepo("releases")

resolvers += Resolver.sonatypeRepo("snapshots")

val commonSettings = Seq(
  scalaVersion := "2.11.6",
  scalacOptions := Seq("-feature", "-unchecked")
)

val macrosSettings = commonSettings ++ Seq(
  libraryDependencies <+= scalaVersion("org.scala-lang" % "scala-reflect" % _)
)

val rootSettings = commonSettings ++ Seq(
  scalacOptions ++= Seq("-deprecation", "-Xlint", "-Xfatal-warnings"),
  unmanagedSourceDirectories in Compile ++= {
    val gen = baseDirectory.value / "src" / "gen"
    Seq(gen / "java", gen / "scala")
  },
  initialCommands in console := """import app.models._, world._, implicits._""",
  libraryDependencies ++= Seq(
    "com.typesafe.akka" %% "akka-actor" % "2.3.6",
    "org.scalaz" %% "scalaz-core" % "7.1.0",
    "commons-io" % "commons-io" % "2.4",
    "com.github.t3hnar" %% "scala-bcrypt" % "2.4",
    "com.github.nscala-time" %% "nscala-time" % "1.8.0",
    "com.beachape" %% "enumeratum" % "1.0.1",
    "org.spire-math" %% "spire" % "0.9.0",
    "com.trueaccord.scalapb" %% "scalapb-runtime" % "0.4.8"
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