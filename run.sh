#!/bin/bash
# テトリスを直接実行するスクリプト

cd "$(dirname "$0")"

CLASSPATH="/workspaces/fp-puzzle/target/scala-3.3.1/classes:/home/vscode/.cache/coursier/v1/https/repo1.maven.org/maven2/org/scala-lang/scala3-library_3/3.3.1/scala3-library_3-3.3.1.jar:/home/vscode/.cache/coursier/v1/https/repo1.maven.org/maven2/dev/zio/zio_3/2.0.19/zio_3-2.0.19.jar:/home/vscode/.cache/coursier/v1/https/repo1.maven.org/maven2/dev/zio/zio-streams_3/2.0.19/zio-streams_3-2.0.19.jar:/home/vscode/.cache/coursier/v1/https/repo1.maven.org/maven2/org/jline/jline/3.25.0/jline-3.25.0.jar:/home/vscode/.cache/coursier/v1/https/repo1.maven.org/maven2/org/scala-lang/scala-library/2.13.10/scala-library-2.13.10.jar:/home/vscode/.cache/coursier/v1/https/repo1.maven.org/maven2/dev/zio/zio-internal-macros_3/2.0.19/zio-internal-macros_3-2.0.19.jar:/home/vscode/.cache/coursier/v1/https/repo1.maven.org/maven2/dev/zio/zio-stacktracer_3/2.0.19/zio-stacktracer_3-2.0.19.jar:/home/vscode/.cache/coursier/v1/https/repo1.maven.org/maven2/dev/zio/izumi-reflect_3/2.3.8/izumi-reflect_3-2.3.8.jar:/home/vscode/.cache/coursier/v1/https/repo1.maven.org/maven2/dev/zio/izumi-reflect-thirdparty-boopickle-shaded_3/2.3.8/izumi-reflect-thirdparty-boopickle-shaded_3-2.3.8.jar"

# ターミナルをリセットしてから実行
clear
exec java -cp "$CLASSPATH" monadris.Main
