name := "TBS Game"

scalaVersion := "2.11.2"

resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"

resolvers += Resolver.sonatypeRepo("releases")

resolvers += Resolver.sonatypeRepo("snapshots")

libraryDependencies += "com.typesafe.akka" %% "akka-actor" % "2.3.6"

libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.1.0"

val monocleLibVer = "0.5.1"

libraryDependencies += "com.chuusai" %% "shapeless" % "2.0.0"

libraryDependencies ++= Seq(
  "com.github.julien-truffaut"  %%  "monocle-core"    % monocleLibVer,
  "com.github.julien-truffaut"  %%  "monocle-generic" % monocleLibVer,
  "com.github.julien-truffaut"  %%  "monocle-macro"   % monocleLibVer,         // since 0.4.0
  "com.github.julien-truffaut"  %%  "monocle-law"     % monocleLibVer % "test" // since 0.4.0
)

libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.2.1" % "test"