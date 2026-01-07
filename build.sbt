val scala3Version = "3.3.5"

val commonSettings = Seq(
  version      := "0.1.0",
  scalaVersion := scala3Version,
  scalacOptions ++= Seq("-deprecation", "-feature", "-Wunused:imports", "-Xfatal-warnings"),
  libraryDependencies ++= Seq(
    "org.scalatest"       %% "scalatest" % "3.2.17" % Test,
    "com.tngtech.archunit" % "archunit"  % "1.2.1"  % Test
  ),
  semanticdbEnabled       := true,
  semanticdbVersion       := scalafixSemanticdb.revision,
  scalafixOnCompile       := true,
  wartremoverCrossVersion := CrossVersion.binary,
  scalafixConfig          := Some(file(".scalafix.conf")),
  Test / scalafixConfig   := Some(file(".scalafix-test.conf"))
)

// 純粋なコアロジック
lazy val core = project
  .in(file("core"))
  .settings(commonSettings)
  .settings(
    name := "monadris-core",
    // ZIOなどのライブラリ依存は一切なし
    // WartRemover: 純粋関数型を強制する (Compile スコープのみ)
    Compile / wartremoverErrors ++= Warts.unsafe.filterNot(_ == Wart.DefaultArguments), // DefaultArgumentsのみ許容
    // 明示的に禁止を追加
    Compile / wartremoverErrors ++= Seq(Wart.Var, Wart.Null, Wart.Return, Wart.Throw),
    // テストコードではWartRemoverを除外
    wartremoverExcluded += (Test / sourceDirectory).value
  )

// アプリケーション本体 (Coreに依存 + ZIO)
lazy val app = project
  .in(file("app"))
  .dependsOn(core)
  .enablePlugins(JavaAppPackaging)
  .settings(commonSettings)
  .settings(
    name := "monadris-app",
    libraryDependencies ++= Seq(
      "dev.zio"       %% "zio"                 % "2.0.19",
      "dev.zio"       %% "zio-streams"         % "2.0.19",
      "dev.zio"       %% "zio-logging"         % "2.1.15",
      "dev.zio"       %% "zio-logging-slf4j"   % "2.1.15",
      "dev.zio"       %% "zio-config"          % "4.0.0-RC16",
      "dev.zio"       %% "zio-config-typesafe" % "4.0.0-RC16",
      "dev.zio"       %% "zio-config-magnolia" % "4.0.0-RC16",
      "ch.qos.logback" % "logback-classic"     % "1.5.24",
      "dev.zio"       %% "zio-test"            % "2.0.19" % Test,
      "dev.zio"       %% "zio-test-sbt"        % "2.0.19" % Test,
      "dev.zio"       %% "zio-test-magnolia"   % "2.0.19" % Test
    ),
    testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework"),
    fork         := true,
    connectInput := true,
    // app層ではWartRemoverは無効化
    wartremoverErrors                      := Seq.empty,
    Compile / doc / sources                := Seq.empty,
    Compile / packageDoc / publishArtifact := false
  )

lazy val root = project
  .in(file("."))
  .aggregate(core, app)
  .settings(
    name           := "monadris",
    publish / skip := true
  )
