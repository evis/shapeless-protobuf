name := "shapeless-protobuf"

version := "0.1"

scalaVersion := "2.12.6"

scalacOptions += "-Xlog-implicits"
scalacOptions += "-Xprint:typer"

libraryDependencies ++= Seq(
  "org.scala-lang" % "scala-compiler" % scalaVersion.toString,
  "org.scala-lang" % "scala-reflect" % scalaVersion.toString,
  "com.chuusai" %% "shapeless" % "2.3.3",
  "org.scalacheck" %% "scalacheck" % "1.14.0" % "test",
  "com.github.alexarchambault" %% "scalacheck-shapeless_1.13" % "1.1.6" % "test",
  "com.google.protobuf" % "protobuf-java" % "3.5.1" % "test",
)

addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)
