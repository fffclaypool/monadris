FROM mcr.microsoft.com/devcontainers/java:1-21-bullseye

RUN curl -fsSL https://deb.nodesource.com/setup_20.x | bash - && \
    apt-get install -y nodejs && \
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
