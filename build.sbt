name := "shapeless-protobuf"

version := "0.1"

scalaVersion := "2.12.6"

scalacOptions += "-Xlog-implicits"
scalacOptions += "-Xprint:typer"

libraryDependencies ++= Seq(
  "org.scala-lang" % "scala-reflect" % scalaVersion.toString,
  "com.chuusai" %% "shapeless" % "2.3.3",
  "com.google.protobuf" % "protobuf-java" % "3.5.1",
  "com.github.alexarchambault" %% "scalacheck-shapeless_1.13" % "1.1.6",
)

addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)
