# ベースイメージ: Java 21
FROM mcr.microsoft.com/devcontainers/java:1-21-bullseye

# 1. 必要なツールのインストール (Node.js & Scala)
# TARGETARCH を利用して Coursier のアーキテクチャを決定
ARG TARGETARCH
RUN export CS_ARCH=$([ "$TARGETARCH" = "arm64" ] && echo "aarch64" || echo "x86_64") && \
    curl -fL "https://github.com/coursier/launchers/raw/master/cs-${CS_ARCH}-pc-linux.gz" | gzip -d > cs && \
    chmod +x cs && \
    ./cs setup --yes --apps sbt,scala,scala-cli,scalafmt --install-dir /usr/local/bin && \
    rm cs

# 2. Node.js (Claude Code の実行に必要) と Claude Code 本体のインストール
# 公式のインストーラーを使用してグローバルにインストール
RUN curl -fsSL https://deb.nodesource.com/setup_20.x | bash - && \
    apt-get install -y nodejs && \
    npm install -g @anthropic-ai/claude-code

USER vscode
