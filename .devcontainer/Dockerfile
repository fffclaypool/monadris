FROM mcr.microsoft.com/devcontainers/java:1-21-bullseye

USER root
RUN rm -f /etc/apt/sources.list.d/yarn.list \
    && apt-get update \
    && apt-get install -y --no-install-recommends gnupg2 \
    && curl -fsSL https://deb.nodesource.com/setup_20.x | bash - \
    && mkdir -p /etc/apt/keyrings \
    && curl -fsSL https://repo.charm.sh/apt/gpg.key | gpg --dearmor -o /etc/apt/keyrings/charm.gpg \
    && echo "deb [signed-by=/etc/apt/keyrings/charm.gpg] https://repo.charm.sh/apt/ * *" | tee /etc/apt/sources.list.d/charm.list \
    && apt-get update \
    && apt-get install -y --no-install-recommends \
        chromium \
        ffmpeg \
        nodejs \
        postgresql-client \
        python3-pip \
        vhs \
        docker.io \
    && npm install -g @anthropic-ai/claude-code repomix \
    && pip3 install --no-cache-dir pre-commit \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

RUN usermod -aG docker vscode

USER vscode
WORKDIR /home/vscode

RUN mkdir -p /home/vscode/.ivy2

ARG TARGETARCH
RUN CS_ARCH=$([ "$TARGETARCH" = "arm64" ] && echo "aarch64" || echo "x86_64") \
    && curl -fsSL "https://github.com/coursier/launchers/raw/master/cs-${CS_ARCH}-pc-linux.gz" | gzip -d > cs \
    && chmod +x cs \
    && ./cs setup --yes --apps sbt,scala,scala-cli,scalafmt,cs \
    && rm cs

ENV PATH="/home/vscode/.local/share/coursier/bin:${PATH}"
