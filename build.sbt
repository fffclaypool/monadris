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
      "dev.zio" %% "zio-logging" % "2.1.15",
      "dev.zio" %% "zio-logging-slf4j" % "2.1.15",
      "dev.zio" %% "zio-config" % "4.0.0-RC16",
      "dev.zio" %% "zio-config-typesafe" % "4.0.0-RC16",
      "dev.zio" %% "zio-config-magnolia" % "4.0.0-RC16",
      "ch.qos.logback" % "logback-classic" % "1.4.14",
      "dev.zio" %% "zio-test" % "2.0.19" % Test,
      "dev.zio" %% "zio-test-sbt" % "2.0.19" % Test,
      "dev.zio" %% "zio-test-magnolia" % "2.0.19" % Test,
      "org.scalatest" %% "scalatest" % "3.2.17" % Test
    ),
    testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework"),
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
    // Coverage exclusions for untestable IO code (Main entry point)
    coverageExcludedFiles := ".*Main\\.scala"
  )
