#!/usr/bin/env bash
#
# Ubuntu 24.04 開発環境セットアップ ブートストラップスクリプト
# 使用方法: ./bootstrap.sh
#
set -euo pipefail

# 色付き出力
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

log_info() { echo -e "${BLUE}[INFO]${NC} $1"; }
log_success() { echo -e "${GREEN}[OK]${NC} $1"; }
log_warn() { echo -e "${YELLOW}[WARN]${NC} $1"; }
log_error() { echo -e "${RED}[ERROR]${NC} $1"; }

# スクリプトのディレクトリを取得
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$SCRIPT_DIR"

echo ""
echo "=========================================="
echo "  Ubuntu 24.04 開発環境セットアップ"
echo "=========================================="
echo ""

# Ubuntu 24.04かチェック
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

# システムアップデート
log_info "システムをアップデート中..."
sudo apt update && sudo apt upgrade -y

# 基本パッケージインストール
log_info "基本パッケージをインストール中..."
sudo apt install -y \
    software-properties-common \
    apt-transport-https \
    ca-certificates \
    curl \
    wget \
    git \
    gnupg \
    lsb-release

# Ansibleインストール
if command -v ansible &> /dev/null; then
    log_success "Ansible は既にインストール済み: $(ansible --version | head -1)"
else
    log_info "Ansibleをインストール中..."
    sudo apt-add-repository --yes --update ppa:ansible/ansible
    sudo apt install -y ansible
    log_success "Ansible インストール完了"
fi

# ansible.cfg が存在するか確認
if [[ ! -f "$SCRIPT_DIR/ansible.cfg" ]]; then
    log_error "ansible.cfg が見つかりません: $SCRIPT_DIR/ansible.cfg"
    exit 1
fi

# playbook.yml が存在するか確認
if [[ ! -f "$SCRIPT_DIR/playbook.yml" ]]; then
    log_error "playbook.yml が見つかりません: $SCRIPT_DIR/playbook.yml"
    exit 1
fi

echo ""
log_info "Ansible Playbookを実行します..."
echo ""

# 実行モード選択
echo "実行モードを選択してください:"
echo "  1) フルセットアップ（全て）"
echo "  2) 基本のみ（base, git, terminal）"
echo "  3) 開発環境（editors, python, nodejs, go, java, clojure）"
echo "  4) インフラ（database, docker）"
echo "  5) 日本語・ユーティリティ（japanese, utilities）"
echo "  6) ドライラン（変更確認のみ）"
echo "  7) カスタム（タグを指定）"
echo ""
read -p "選択 [1-7]: " choice

case $choice in
    1)
        ansible-playbook playbook.yml
        ;;
    2)
        ansible-playbook playbook.yml --tags "base,git,terminal"
        ;;
    3)
        ansible-playbook playbook.yml --tags "editors,python,nodejs,go,java,clojure"
        ;;
    4)
        ansible-playbook playbook.yml --tags "database,docker"
        ;;
    5)
        ansible-playbook playbook.yml --tags "japanese,utilities"
        ;;
    6)
        ansible-playbook playbook.yml --check -v
        ;;
    7)
        read -p "タグをカンマ区切りで入力: " custom_tags
        ansible-playbook playbook.yml --tags "$custom_tags"
        ;;
    *)
        log_error "無効な選択です"
        exit 1
        ;;
esac

echo ""
log_success "セットアップ完了!"
echo ""
echo "次のステップ:"
echo "  1. ターミナルを再起動（または source ~/.bashrc）"
echo "  2. 必要に応じて dotfiles を設定"
echo "  3. gh auth login でGitHub認証"
echo ""
