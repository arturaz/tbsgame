name := "TBS Game"

scalaVersion := "2.11.6"

scalacOptions := Seq(
  "-feature", "-deprecation", "-unchecked", "-Xlint", "-Xfatal-warnings"
)

unmanagedSourceDirectories in Compile += baseDirectory.value / "src" / "gen" / "java"

initialCommands in console := """import app.models._, world._, implicits._"""

resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"

resolvers += Resolver.sonatypeRepo("releases")

resolvers += Resolver.sonatypeRepo("snapshots")

libraryDependencies += "com.typesafe.akka" %% "akka-actor" % "2.3.6"

libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.1.0"

libraryDependencies += "commons-io" % "commons-io" % "2.4"

libraryDependencies ++= Seq(
  "org.flywaydb" % "flyway-core" % "3.1",
  "com.zaxxer" % "HikariCP-java6" % "2.2.5",
  "org.xerial" % "sqlite-jdbc" % "3.8.7",
  "com.typesafe.slick" %% "slick" % "2.1.0"
)

libraryDependencies += "com.github.t3hnar" %% "scala-bcrypt" % "2.4"

val monocleLibVer = "0.5.1"

libraryDependencies += "com.chuusai" %% "shapeless" % "2.0.0"

libraryDependencies ++= Seq(
  "com.github.julien-truffaut"  %%  "monocle-core"    % monocleLibVer,
  "com.github.julien-truffaut"  %%  "monocle-generic" % monocleLibVer,
  "com.github.julien-truffaut"  %%  "monocle-macro"   % monocleLibVer,         // since 0.4.0
  "com.github.julien-truffaut"  %%  "monocle-law"     % monocleLibVer % "test" // since 0.4.0
)

libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.2.1" % "test"

libraryDependencies += "com.google.protobuf" % "protobuf-java" % "2.4.1"

libraryDependencies += "com.github.nscala-time" %% "nscala-time" % "1.8.0"