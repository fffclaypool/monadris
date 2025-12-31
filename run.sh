#!/bin/bash
cd "$(dirname "$0")"

# 1. マーカーファイル（前回のビルド成功時刻を記録するファイル）
BUILD_MARKER=".last_build_success"

# 2. 実行するスクリプトのパス
#    (ファイル名が変わっても対応できるように自動検出)
TARGET_SCRIPT=$(find ./app/target/universal/stage/bin -type f ! -name "*.bat" 2>/dev/null | head -n 1)

# 3. ビルドが必要か判定する関数
needs_build() {
    # マーカーがない、または起動スクリプトがない場合はビルド必須
    if [ ! -f "$BUILD_MARKER" ] || [ -z "$TARGET_SCRIPT" ] || [ ! -f "$TARGET_SCRIPT" ]; then
        return 0
    fi

    # ソースコードの変更チェック
    # -path "*/target/*" -prune : targetディレクトリの中身は無視する
    # -newer "$BUILD_MARKER"    : 前回のビルド成功より新しいファイルを探す
    CHANGED=$(find app/src core/src project build.sbt \
        -name "target" -prune -o \
        -name ".*" -prune -o \
        -type f -newer "$BUILD_MARKER" -print -quit)

    if [ -n "$CHANGED" ]; then
        # 変更が見つかった場合（デバッグ用にファイル名を表示してもよい）
        # echo "Change detected: $CHANGED"
        return 0
    else
        return 1
    fi
}

# --- メイン処理 ---
if needs_build; then
    echo "Building..."
    sbt --error app/stage

    if [ $? -ne 0 ]; then
        echo "Build failed."
        exit 1
    fi

    # ★ビルド成功時にマーカーファイルの時刻を更新
    touch "$BUILD_MARKER"

    # スクリプトパスを再取得
    TARGET_SCRIPT=$(find ./app/target/universal/stage/bin -type f ! -name "*.bat" | head -n 1)
else
    echo "No changes. Skipping build."
    # メッセージが一瞬で見えるように少し待機
    sleep 0.2
fi

clear

# 実行
"$TARGET_SCRIPT"
