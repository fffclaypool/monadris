val scala3Version = "3.3.1"

lazy val root = project
  .in(file("."))
  .settings(
    name := "monadris",
    version := "0.1.0",
    scalaVersion := scala3Version,
    libraryDependencies ++= Seq(
      "dev.zio" %% "zio" % "2.0.19",
      "dev.zio" %% "zio-streams" % "2.0.19",
      "org.jline" % "jline" % "3.25.0",
      "org.scalatest" %% "scalatest" % "3.2.17" % Test
    ),
    fork := true,
    connectInput := true,
    scalacOptions ++= Seq(
      "-deprecation",
      "-feature",
      "-Wunused:imports",
      "-Xfatal-warnings"
    ),
    semanticdbEnabled := true,
    semanticdbVersion := scalafixSemanticdb.revision,
  )
