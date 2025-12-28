FROM mcr.microsoft.com/devcontainers/java:1-21-bullseye

ARG TARGETARCH

RUN export CS_ARCH=$([ "$TARGETARCH" = "arm64" ] && echo "aarch64" || echo "x86_64") && \
    curl -fL "https://github.com/coursier/launchers/raw/master/cs-${CS_ARCH}-pc-linux.gz" | gzip -d > cs && \
    chmod +x cs && \
    ./cs setup --yes --apps sbt,scala,scala-cli,scalafmt --install-dir /usr/local/bin && \
    rm cs

RUN curl -fsSL https://deb.nodesource.com/setup_20.x | bash - && \
    apt-get install -y nodejs && \
    npm install -g @anthropic-ai/claude-code && \
    apt-get clean && \
    rm -rf /var/lib/apt/lists/*

USER vscode
