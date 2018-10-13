import sbt._
import Keys._

val paradiseVersion = "2.1.0"
val buildSettings = Defaults.coreDefaultSettings ++ Seq(
  organization := "com.evis.github",
  version := "0.1.0",
  scalaVersion := "2.12.6",
  scalacOptions ++= Seq(
    "-target:jvm-1.8",
    "-encoding", "UTF-8",
    "-unchecked",
    "-deprecation",
    "-Xfuture",
    "-Yno-adapted-args",
    "-Ywarn-dead-code",
    "-Ywarn-numeric-widen",
    "-Ywarn-value-discard",
    "-Ywarn-unused",
    "-feature",
    "-language:experimental.macros",
    "-Xlint:_",
    "-Xfatal-warnings"
  ),
  crossScalaVersions := Seq("2.10.2", "2.10.3", "2.10.4", "2.10.5", "2.10.6",
    "2.11.0", "2.11.1", "2.11.2", "2.11.3", "2.11.4",
    "2.11.5", "2.11.6", "2.11.7", "2.11.8",
    "2.12.0", "2.12.1", "2.12.2", "2.12.3", "2.12.4", "2.12.5", "2.12.6"),
  resolvers += Resolver.sonatypeRepo("snapshots"),
  resolvers += Resolver.sonatypeRepo("releases"),
  addCompilerPlugin("org.scalamacros" % "paradise" % paradiseVersion cross CrossVersion.full)
)

lazy val root: Project = {
  project.in(file("."))
    .settings(buildSettings ++ Seq(run := (run in Compile in core).evaluated))
    .aggregate(macros, core)
}

lazy val macros: Project = {
  Project("macros", file("macros"))
    .settings(buildSettings ++ Seq(
      libraryDependencies += scalaVersion("org.scala-lang" % "scala-compiler" % _).value,
      libraryDependencies += scalaVersion("org.scala-lang" % "scala-reflect" % _).value,
      libraryDependencies += "com.chuusai" %% "shapeless" % "2.3.3",
      libraryDependencies ++= (
        if (scalaVersion.value.startsWith("2.10")) List("org.scalamacros" %% "quasiquotes" % paradiseVersion)
        else Nil
        )
      )
    )
}

lazy val core: Project = {
  Project("core", file("core"))
    .enablePlugins(ProtobufTestPlugin)
    .settings(buildSettings ++ Seq(
      libraryDependencies ++= Seq(
        "org.scalacheck" %% "scalacheck" % "1.14.0" % "test",
        "com.github.alexarchambault" %% "scalacheck-shapeless_1.13" % "1.1.6" % "test",
      ),
    ))
    .dependsOn(macros)
}
