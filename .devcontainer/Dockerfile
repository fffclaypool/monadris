FROM mcr.microsoft.com/devcontainers/java:1-21-bullseye

RUN apt-get update && apt-get install -y gnupg2 && \
    curl -fsSL https://deb.nodesource.com/setup_20.x | bash - && \
    mkdir -p /etc/apt/keyrings && \
    curl -fsSL https://repo.charm.sh/apt/gpg.key | gpg --dearmor -o /etc/apt/keyrings/charm.gpg && \
    echo "deb [signed-by=/etc/apt/keyrings/charm.gpg] https://repo.charm.sh/apt/ * *" | tee /etc/apt/sources.list.d/charm.list && \
    apt-get update && \
    apt-get install -y nodejs vhs ffmpeg chromium && \
    npm install -g @anthropic-ai/claude-code repomix && \
    apt-get clean && \
    rm -rf /var/lib/apt/lists/*

USER vscode
WORKDIR /home/vscode

ARG TARGETARCH

RUN export CS_ARCH=$([ "$TARGETARCH" = "arm64" ] && echo "aarch64" || echo "x86_64") && \
    curl -fL "https://github.com/coursier/launchers/raw/master/cs-${CS_ARCH}-pc-linux.gz" | gzip -d > cs && \
    chmod +x cs && \
    ./cs setup --yes --apps sbt,scala,scala-cli,scalafmt,cs && \
    rm cs

ENV PATH="/home/vscode/.local/share/coursier/bin:${PATH}"
