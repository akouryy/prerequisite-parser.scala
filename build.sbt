val dottyVersion = "0.27.0-RC1"

lazy val root = project
  .in(file("."))
  .settings(
    organization := "app.pyon",
    name := "prerequisite-parser-scala",
    version := "1.0.0",

    scalaVersion := dottyVersion,

    scalacOptions ++= Seq(
      "-deprecation",
      "-feature",
      "-language:strictEquality",
      "-migration",
      "-unchecked",
      "-Yexplicit-nulls",
      "-Yindent-colons",
    ),

    libraryDependencies ++= Seq(
      "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.2" withDottyCompat dottyVersion,
    ),

    publishTo := Some(
      Resolver.file(name.value, file("docs"))(
        Patterns(true, Resolver.mavenStyleBasePattern),
      )
    )
  )
