#!/bin/bash
# setup-private.sh - personal setup script for container

SERVICE_TYPE=$1

setup_tools() {
    # 任意のコマンドをdocker compose up時に実行できる
    # apt install -y tree jq ripgrep fzf neovim
    # curl -sS https://starship.rs/install.sh | sh -s -- --yes
}

# serverサービス用のNode.jsセットアップ
setup_nodejs_for_server() {
    if ! command -v node > /dev/null 2>&1; then
        echo "[Claude Setup] Installing Node.js..."
        curl -fsSL https://deb.nodesource.com/setup_22.x -o /tmp/nodesource_setup.sh
        bash /tmp/nodesource_setup.sh
        apt-get install -y nodejs
        rm /tmp/nodesource_setup.sh
    fi
}

# Claude Codeのインストール（共通）
install_claude() {
    if ! command -v claude > /dev/null 2>&1; then
        echo "[Claude Setup] Installing Claude Code..."
        npm install -g @anthropic-ai/claude-code
    fi
}

setup_tools

# メイン処理
case "$SERVICE_TYPE" in
    "server")
        setup_nodejs_for_server
        install_claude
        exec clojure -M:api:test:repl
        ;;
    "frontend")
        install_claude
        exec yarn dev
        ;;
esac