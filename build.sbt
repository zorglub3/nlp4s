ThisBuild / scalaVersion     := "2.13.16"
ThisBuild / semanticdbEnabled := true
ThisBuild / semanticdbVersion := scalafixSemanticdb.revision
ThisBuild / version          := "0.1.0"
ThisBuild / organization     := "nlp4s"

addCompilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.1")

lazy val dependencies = Seq(
    "org.scalatest" %% "scalatest" % "3.1.2" % Test
  , "org.scala-graph" %% "graph-core" % "2.0.0"
  , "org.scala-lang.modules" %% "scala-collection-contrib" % "0.3.0"
  , "org.typelevel" %% "cats-core" % "2.13.0"
)

lazy val root = (project in file("."))
  .settings(
      name := "nlp4s",
      javacOptions ++= Seq(
        "-Xlint",
        "-encoding", "UTF-8"
        ),
      scalacOptions ++= Seq(
        "-Xlint",
        "-Ywarn-dead-code",
        "-Wunused:imports",
        "-Ywarn-numeric-widen",
        "-unchecked",
        "-deprecation",
        "-feature",
        "-encoding", "UTF-8"
        ),
      exportJars := true,
      libraryDependencies ++= dependencies
  )


