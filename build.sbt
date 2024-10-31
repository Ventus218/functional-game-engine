val scala3Version = "3.5.2"

lazy val root = project
  .in(file("."))
  .settings(
    name := "functional game engine",
    version := "0.1.0-SNAPSHOT",

    scalaVersion := scala3Version,

    libraryDependencies += "org.typelevel" %% "cats-core" % "2.9.0",
    libraryDependencies += "org.typelevel" %% "cats-effect" % "2.5.3",
    libraryDependencies += "org.scalameta" %% "munit" % "1.0.0" % Test
  )
