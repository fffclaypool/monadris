#!/bin/bash
cd "$(dirname "$0")"

BUILD_MARKER=".last_build_success"
TARGET_SCRIPT=$(find ./app/target/universal/stage/bin -type f ! -name "*.bat" 2>/dev/null | head -n 1)

needs_build() {
    if [ ! -f "$BUILD_MARKER" ] || [ -z "$TARGET_SCRIPT" ] || [ ! -f "$TARGET_SCRIPT" ]; then
        return 0
    fi
    CHANGED=$(find app/src core/src project build.sbt \
        -name "target" -prune -o \
        -name ".*" -prune -o \
        -type f -newer "$BUILD_MARKER" -print -quit)
    if [ -n "$CHANGED" ]; then
        return 0
    else
        return 1
    fi
}

if needs_build; then
    echo "Building..."
    sbt --error app/stage
    if [ $? -ne 0 ]; then
        echo "Build failed."
        exit 1
    fi
    touch "$BUILD_MARKER"
    TARGET_SCRIPT=$(find ./app/target/universal/stage/bin -type f ! -name "*.bat" | head -n 1)
else
    echo "No changes. Skipping build."
    sleep 0.2
fi

clear

"$TARGET_SCRIPT"
