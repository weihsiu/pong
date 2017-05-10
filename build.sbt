enablePlugins(ScalaJSPlugin)

name := "pong"
version := "0.0.1-SNAPSHOT"
scalaVersion := "2.12.2"

scalacOptions ++= Seq(
  "-feature",
  "-Ypartial-unification"
)

addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.3")

resolvers += Resolver.sonatypeRepo("snapshots")

libraryDependencies ++= Seq(
  "org.typelevel" %%% "cats-core" % "0.9.0",
  "org.typelevel" %%% "cats-effect" % "0.1-0848c9b",
  "org.atnos" %%% "eff" % "4.3.1",
  "org.atnos" %%% "eff-monix" % "4.3.1",
  "co.fs2" %%% "fs2-core" % "1.0.0-SNAPSHOT",
  "org.scala-js" %%% "scalajs-dom" % "0.9.1"
)

scalaJSUseMainModuleInitializer := true
mainClass in Compile := Some("pong.PongMain")