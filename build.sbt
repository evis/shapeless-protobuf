name := "shapeless-protobuf"

version := "0.1"

scalaVersion := "2.12.6"

lazy val testMessages = {
  (project in file("test-messages"))
    .settings(
      name := "shapeless-protobuf-test-messages",
      libraryDependencies ++= Seq(
        "com.google.protobuf" % "protobuf-java" % "3.5.1",
      ),
    )
}

lazy val core = {
  (project in file("core"))
    .dependsOn(testMessages % "test->test")
    .settings(
      name := "shapeless-protobuf-core",
      libraryDependencies ++= Seq(
        "org.scala-lang" % "scala-compiler" % scalaVersion.toString,
        "org.scala-lang" % "scala-reflect" % scalaVersion.toString,
        "com.chuusai" %% "shapeless" % "2.3.3",
        "org.scalacheck" %% "scalacheck" % "1.14.0" % "test",
        "com.github.alexarchambault" %% "scalacheck-shapeless_1.13" % "1.1.6" % "test",
      ),
      scalacOptions ++= Seq(
        "-Xlog-implicits",
        "-Xprint:typer",
      ),
      addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full),
    )
}
