ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.1.3"

lazy val root = (project in file("."))
  .settings(
    name := "PokemonDB"
  )

  libraryDependencies ++= Seq(
    "io.spray" %%  "spray-json" % "1.3.6",
    "com.squareup.okhttp3" % "okhttp" % "4.10.0",
    "io.monix" %% "monix-execution" % "3.4.0",
    "io.monix" %% "monix-eval" % "3.4.0",
    "org.scalactic" %% "scalactic" % "3.2.12",
    "org.scalatest" %% "scalatest" % "3.2.12" % "test"
  )