#!/usr/bin/env bash
#
# Ubuntu 24.04 開発環境セットアップスクリプト
# 生成日: 2026-01-06
#
set -euo pipefail

# ============================================
# 共通関数
# ============================================
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m'

log_info()    { echo -e "${BLUE}[INFO]${NC} $1"; }
log_success() { echo -e "${GREEN}[OK]${NC} $1"; }
log_warn()    { echo -e "${YELLOW}[WARN]${NC} $1"; }
log_error()   { echo -e "${RED}[ERROR]${NC} $1"; }

command_exists() { command -v "$1" &> /dev/null; }

# ============================================
# 設定変数（必要に応じて変更）
# ============================================
GIT_USER_NAME="Your Name"
GIT_USER_EMAIL="your@email.com"
PYTHON_VERSION="3.12.0"
GO_VERSION="1.23.0"

echo ""
echo "=========================================="
echo "  Ubuntu 24.04 開発環境セットアップ"
echo "=========================================="
echo ""

# Ubuntu チェック
if [[ -f /etc/os-release ]]; then
    . /etc/os-release
    if [[ "$ID" != "ubuntu" ]]; then
        log_error "このスクリプトはUbuntu専用です (検出: $ID)"
        exit 1
    fi
    log_info "検出: $PRETTY_NAME"
else
    log_error "/etc/os-release が見つかりません"
    exit 1
fi

# ~/.local/bin 作成
mkdir -p "$HOME/.local/bin"
