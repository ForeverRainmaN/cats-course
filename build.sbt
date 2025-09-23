ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / scalaVersion := "3.6.2"

lazy val root = (project in file("."))
  .settings(
    name := "cats-course",
    scalacOptions ++= Seq(
      "-language:higherKinds",
    )
  )

val catsVersion = "2.12.0"

libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-core" % catsVersion
)