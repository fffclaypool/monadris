val scala3Version = "3.3.5"

val commonSettings = Seq(
  version      := "0.1.0",
  scalaVersion := scala3Version,
  scalacOptions ++= Seq("-deprecation", "-feature", "-Wunused:imports", "-Xfatal-warnings"),
  libraryDependencies ++= Seq(
    "org.scalatest"       %% "scalatest" % "3.2.19" % Test,
    "com.tngtech.archunit" % "archunit"  % "1.4.1"  % Test
  ),
  semanticdbEnabled       := true,
  semanticdbVersion       := scalafixSemanticdb.revision,
  scalafixOnCompile       := true,
  wartremoverCrossVersion := CrossVersion.binary,
  scalafixConfig          := Some(file(".scalafix.conf")),
  Test / scalafixConfig   := Some(file(".scalafix-test.conf"))
)

lazy val core = project
  .in(file("core"))
  .settings(commonSettings)
  .settings(
    name := "monadris-core",
    Compile / wartremoverErrors ++= Warts.unsafe.filterNot(_ == Wart.DefaultArguments), // DefaultArgumentsのみ許容
    Compile / wartremoverErrors ++= Seq(Wart.Var, Wart.Null, Wart.Return, Wart.Throw),
    wartremoverExcluded += (Test / sourceDirectory).value
  )

lazy val Stress      = config("stress") extend Test
lazy val Integration = config("integration") extend Test

lazy val app = project
  .in(file("app"))
  .dependsOn(core)
  .enablePlugins(JavaAppPackaging)
  .configs(Stress, Integration)
  .settings(commonSettings)
  .settings(
    inConfig(Stress)(Defaults.testSettings),
    Stress / sourceDirectory := (Test / sourceDirectory).value,
    Stress / envVars         := Map("RUN_STRESS_TESTS" -> "1"),
    Stress / fork            := true,
    Stress / testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework"),
    inConfig(Integration)(Defaults.testSettings),
    Integration / sourceDirectory := (Test / sourceDirectory).value,
    Integration / fork            := true,
    Integration / testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework"),
    Integration / testOptions := Seq.empty,
    name                      := "monadris-app",
    libraryDependencies ++= Seq(
      "dev.zio"       %% "zio"                        % "2.1.24",
      "dev.zio"       %% "zio-streams"                % "2.1.24",
      "dev.zio"       %% "zio-logging"                % "2.1.17",
      "dev.zio"       %% "zio-logging-slf4j"          % "2.1.17",
      "dev.zio"       %% "zio-config"                 % "4.0.0-RC16",
      "dev.zio"       %% "zio-config-typesafe"        % "4.0.0-RC16",
      "dev.zio"       %% "zio-config-magnolia"        % "4.0.0-RC16",
      "dev.zio"       %% "zio-json"                   % "0.9.0",
      "ch.qos.logback" % "logback-classic"            % "1.5.32",
      "io.getquill"   %% "quill-jdbc-zio"             % "4.8.6",
      "org.postgresql" % "postgresql"                 % "42.7.10",
      "org.flywaydb"   % "flyway-core"                % "10.7.2",
      "org.flywaydb"   % "flyway-database-postgresql" % "10.7.2",
      "dev.zio"       %% "zio-test"                   % "2.1.24" % Test,
      "dev.zio"       %% "zio-test-sbt"               % "2.1.24" % Test,
      "dev.zio"       %% "zio-test-magnolia"          % "2.1.24" % Test
    ),
    testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework"),
    Test / testOptions += Tests
      .Exclude(Seq("monadris.infrastructure.persistence.PostgresReplayRepositoryIntegrationSpec")),
    fork                                   := true,
    connectInput                           := true,
    wartremoverErrors                      := Seq.empty,
    Compile / doc / sources                := Seq.empty,
    Compile / packageDoc / publishArtifact := false
  )

lazy val root = project
  .in(file("."))
  .aggregate(core, app)
  .settings(
    name                    := "monadris",
    publish / skip          := true,
    scalaVersion            := scala3Version,
    wartremoverCrossVersion := CrossVersion.binary
  )

addCommandAlias("stressTest", "app/Stress/testOnly *StressTest")
addCommandAlias("integrationTest", "app/Integration/testOnly *PostgresReplayRepositoryIntegrationSpec")
