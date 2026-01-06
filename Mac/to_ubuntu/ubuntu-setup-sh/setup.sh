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

# ============================================
# 1. 基本パッケージ
# ============================================
log_info "基本パッケージをインストール中..."

sudo apt update && sudo apt upgrade -y

sudo apt install -y \
    build-essential \
    curl \
    wget \
    git \
    unzip \
    zip \
    tree \
    htop \
    make \
    libssl-dev \
    zlib1g-dev \
    libbz2-dev \
    libreadline-dev \
    libsqlite3-dev \
    llvm \
    libncurses5-dev \
    libncursesw5-dev \
    xz-utils \
    tk-dev \
    libffi-dev \
    liblzma-dev \
    software-properties-common \
    apt-transport-https \
    ca-certificates \
    gnupg \
    lsb-release \
    rlwrap

log_success "基本パッケージ完了"

# ============================================
# 2. Git & GitHub CLI
# ============================================
log_info "Git設定中..."

git config --global user.name "$GIT_USER_NAME"
git config --global user.email "$GIT_USER_EMAIL"
git config --global core.editor nvim
git config --global init.defaultBranch main

log_success "Git設定完了"

# GitHub CLI
if command_exists gh; then
    log_success "GitHub CLI は既にインストール済み"
else
    log_info "GitHub CLI をインストール中..."
    curl -fsSL https://cli.github.com/packages/githubcli-archive-keyring.gpg | sudo dd of=/usr/share/keyrings/githubcli-archive-keyring.gpg
    sudo chmod go+r /usr/share/keyrings/githubcli-archive-keyring.gpg
    echo "deb [arch=$(dpkg --print-architecture) signed-by=/usr/share/keyrings/githubcli-archive-keyring.gpg] https://cli.github.com/packages stable main" | sudo tee /etc/apt/sources.list.d/github-cli.list > /dev/null
    sudo apt update
    sudo apt install -y gh
    log_success "GitHub CLI インストール完了"
fi

# ============================================
# 3. ターミナルツール (fzf, ripgrep, Starship)
# ============================================
log_info "ターミナルツールをインストール中..."

# fzf
if command_exists fzf; then
    log_success "fzf は既にインストール済み"
else
    sudo apt install -y fzf
    log_success "fzf インストール完了"
fi

# ripgrep
if command_exists rg; then
    log_success "ripgrep は既にインストール済み"
else
    sudo apt install -y ripgrep
    log_success "ripgrep インストール完了"
fi

# Starship
if command_exists starship; then
    log_success "Starship は既にインストール済み"
else
    log_info "Starship をインストール中..."
    curl -sS https://starship.rs/install.sh | sh -s -- -y
    log_success "Starship インストール完了"
fi

# bashrc に Starship 追加
if ! grep -q 'eval "$(starship init bash)"' "$HOME/.bashrc"; then
    echo 'eval "$(starship init bash)"' >> "$HOME/.bashrc"
    log_info "Starship を .bashrc に追加"
fi

# ~/.local/bin を PATH に追加
if ! grep -q 'export PATH="$HOME/.local/bin:$PATH"' "$HOME/.bashrc"; then
    echo 'export PATH="$HOME/.local/bin:$PATH"' >> "$HOME/.bashrc"
    log_info "~/.local/bin を PATH に追加"
fi

# ============================================
# 4. エディタ (Neovim, Emacs)
# ============================================

# Neovim
if command_exists nvim; then
    log_success "Neovim は既にインストール済み"
else
    log_info "Neovim をインストール中..."
    sudo add-apt-repository -y ppa:neovim-ppa/unstable
    sudo apt update
    sudo apt install -y neovim
    log_success "Neovim インストール完了"
fi

# Emacs
if command_exists emacs; then
    log_success "Emacs は既にインストール済み"
else
    log_info "Emacs をインストール中..."
    sudo add-apt-repository -y ppa:ubuntuhandbook1/emacs
    sudo apt update
    sudo apt install -y emacs
    log_success "Emacs インストール完了"
fi

# ============================================
# 5. Python (pyenv)
# ============================================
if [[ -d "$HOME/.pyenv" ]]; then
    log_success "pyenv は既にインストール済み"
else
    log_info "pyenv をインストール中..."
    curl https://pyenv.run | bash
    log_success "pyenv インストール完了"
fi

# bashrc に pyenv 追加
if ! grep -q 'PYENV_ROOT' "$HOME/.bashrc"; then
    cat >> "$HOME/.bashrc" << 'EOF'

# pyenv
export PYENV_ROOT="$HOME/.pyenv"
[[ -d $PYENV_ROOT/bin ]] && export PATH="$PYENV_ROOT/bin:$PATH"
eval "$(pyenv init -)"
EOF
    log_info "pyenv を .bashrc に追加"
fi

# Python インストール
export PYENV_ROOT="$HOME/.pyenv"
export PATH="$PYENV_ROOT/bin:$PATH"
if [[ -d "$PYENV_ROOT" ]]; then
    eval "$(pyenv init -)"
    if [[ ! -d "$PYENV_ROOT/versions/$PYTHON_VERSION" ]]; then
        log_info "Python $PYTHON_VERSION をインストール中..."
        pyenv install "$PYTHON_VERSION"
        pyenv global "$PYTHON_VERSION"
        log_success "Python $PYTHON_VERSION インストール完了"
    else
        log_success "Python $PYTHON_VERSION は既にインストール済み"
    fi
fi

# ============================================
# 6. Node.js (Volta)
# ============================================
if [[ -d "$HOME/.volta" ]]; then
    log_success "Volta は既にインストール済み"
else
    log_info "Volta をインストール中..."
    curl https://get.volta.sh | bash -s -- --skip-setup
    log_success "Volta インストール完了"
fi

# bashrc に Volta 追加
if ! grep -q 'VOLTA_HOME' "$HOME/.bashrc"; then
    cat >> "$HOME/.bashrc" << 'EOF'

# Volta
export VOLTA_HOME="$HOME/.volta"
export PATH="$VOLTA_HOME/bin:$PATH"
EOF
    log_info "Volta を .bashrc に追加"
fi

# Node.js インストール
export VOLTA_HOME="$HOME/.volta"
export PATH="$VOLTA_HOME/bin:$PATH"
if [[ -d "$VOLTA_HOME" ]] && [[ ! -f "$VOLTA_HOME/bin/node" ]]; then
    log_info "Node.js (LTS) をインストール中..."
    "$VOLTA_HOME/bin/volta" install node@lts
    log_success "Node.js インストール完了"
else
    log_success "Node.js は既にインストール済み"
fi

# ============================================
# 7. Go
# ============================================
if command_exists go; then
    log_success "Go は既にインストール済み: $(go version)"
else
    log_info "Go $GO_VERSION をインストール中..."
    wget -q "https://go.dev/dl/go${GO_VERSION}.linux-amd64.tar.gz" -O /tmp/go.tar.gz
    sudo rm -rf /usr/local/go
    sudo tar -C /usr/local -xzf /tmp/go.tar.gz
    rm /tmp/go.tar.gz
    log_success "Go インストール完了"
fi

# bashrc に Go 追加
if ! grep -q '/usr/local/go/bin' "$HOME/.bashrc"; then
    cat >> "$HOME/.bashrc" << 'EOF'

# Go
export PATH=$PATH:/usr/local/go/bin
export PATH=$PATH:$HOME/go/bin
EOF
    log_info "Go を .bashrc に追加"
fi

# ============================================
# 8. Java (Temurin OpenJDK)
# ============================================
if command_exists java; then
    log_success "Java は既にインストール済み: $(java -version 2>&1 | head -1)"
else
    log_info "Temurin JDK をインストール中..."

    # Adoptium GPG キー追加
    wget -qO - https://packages.adoptium.net/artifactory/api/gpg/key/public | sudo gpg --dearmor -o /usr/share/keyrings/adoptium.gpg

    # リポジトリ追加
    echo "deb [signed-by=/usr/share/keyrings/adoptium.gpg] https://packages.adoptium.net/artifactory/deb $(lsb_release -cs) main" | sudo tee /etc/apt/sources.list.d/adoptium.list

    sudo apt update

    # 最新LTS (21) をインストール
    sudo apt install -y temurin-21-jdk

    log_success "Temurin JDK インストール完了"
fi

# ============================================
# 9. Clojure エコシステム
# ============================================

# Clojure CLI
if command_exists clj; then
    log_success "Clojure は既にインストール済み"
else
    log_info "Clojure をインストール中..."
    curl -L -O https://github.com/clojure/brew-install/releases/latest/download/linux-install.sh
    chmod +x linux-install.sh
    sudo ./linux-install.sh
    rm linux-install.sh
    log_success "Clojure インストール完了"
fi

# Babashka
if command_exists bb; then
    log_success "Babashka は既にインストール済み"
else
    log_info "Babashka をインストール中..."
    curl -sLO https://raw.githubusercontent.com/babashka/babashka/master/install
    chmod +x install
    sudo ./install
    rm install
    log_success "Babashka インストール完了"
fi

# bbin
if [[ -f "$HOME/.local/bin/bbin" ]]; then
    log_success "bbin は既にインストール済み"
else
    log_info "bbin をインストール中..."
    curl -o- -L https://raw.githubusercontent.com/babashka/bbin/main/install | bash
    log_success "bbin インストール完了"
fi

# clojure-lsp
if command_exists clojure-lsp; then
    log_success "clojure-lsp は既にインストール済み"
else
    log_info "clojure-lsp をインストール中..."
    curl -L -o /tmp/clojure-lsp.zip https://github.com/clojure-lsp/clojure-lsp/releases/latest/download/clojure-lsp-native-linux-amd64.zip
    unzip -o /tmp/clojure-lsp.zip -d /tmp
    sudo mv /tmp/clojure-lsp /usr/local/bin/
    rm /tmp/clojure-lsp.zip
    log_success "clojure-lsp インストール完了"
fi

# clj-kondo
if command_exists clj-kondo; then
    log_success "clj-kondo は既にインストール済み"
else
    log_info "clj-kondo をインストール中..."
    curl -sLO https://raw.githubusercontent.com/clj-kondo/clj-kondo/master/script/install-clj-kondo
    chmod +x install-clj-kondo
    sudo ./install-clj-kondo
    rm install-clj-kondo
    log_success "clj-kondo インストール完了"
fi

# cljfmt (bbin経由)
if [[ -f "$HOME/.local/bin/cljfmt" ]]; then
    log_success "cljfmt は既にインストール済み"
else
    log_info "cljfmt をインストール中..."
    export PATH="$HOME/.local/bin:$PATH"
    if command_exists bbin; then
        bbin install io.github.weavejester/cljfmt
        log_success "cljfmt インストール完了"
    else
        log_warn "bbin が見つかりません。cljfmt をスキップします"
    fi
fi

# ============================================
# 10. 日本語入力 (fcitx5-skk + yaskkserv2)
# ============================================

# fcitx5-skk
log_info "fcitx5-skk をインストール中..."
sudo apt install -y fcitx5 fcitx5-skk im-config

log_info "fcitx5 を既定の入力メソッドに設定中..."
im-config -n fcitx5

# 環境変数を設定
if ! grep -q 'GTK_IM_MODULE=fcitx' "$HOME/.bashrc"; then
    cat >> "$HOME/.bashrc" << 'EOF'

# fcitx5
export GTK_IM_MODULE=fcitx
export QT_IM_MODULE=fcitx
export XMODIFIERS=@im=fcitx
EOF
    log_info "fcitx5 環境変数を .bashrc に追加"
fi

log_success "fcitx5-skk インストール完了"

# Rust (yaskkserv2 用)
if [[ -f "$HOME/.cargo/bin/rustc" ]]; then
    log_success "Rust は既にインストール済み"
else
    log_info "Rust をインストール中..."
    curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- -y
    log_success "Rust インストール完了"
fi

# Cargo 環境読み込み
if [[ -f "$HOME/.cargo/env" ]]; then
    source "$HOME/.cargo/env"
fi

# yaskkserv2
if [[ -f "$HOME/.cargo/bin/yaskkserv2" ]]; then
    log_success "yaskkserv2 は既にインストール済み"
else
    log_info "yaskkserv2 をインストール中..."
    cargo install yaskkserv2
    log_success "yaskkserv2 インストール完了"
fi

# SKK辞書
mkdir -p "$HOME/.skk"
if [[ ! -f "$HOME/.skk/SKK-JISYO.L" ]]; then
    log_info "SKK辞書をダウンロード中..."
    wget -q https://skk-dev.github.io/dict/SKK-JISYO.L.gz -O "$HOME/.skk/SKK-JISYO.L.gz"
    gunzip -f "$HOME/.skk/SKK-JISYO.L.gz"
    log_success "SKK辞書ダウンロード完了"
fi

# yaskkserv2 辞書ビルド
if [[ ! -f "$HOME/.skk/dictionary.yaskkserv2" ]]; then
    log_info "yaskkserv2 辞書をビルド中..."
    "$HOME/.cargo/bin/yaskkserv2_make_dictionary" \
        --utf8 \
        --dictionary-filename "$HOME/.skk/dictionary.yaskkserv2" \
        "$HOME/.skk/SKK-JISYO.L"
    log_success "yaskkserv2 辞書ビルド完了"
fi

# systemd ユーザーサービス作成
mkdir -p "$HOME/.config/systemd/user"
cat > "$HOME/.config/systemd/user/yaskkserv2.service" << 'EOF'
[Unit]
Description=yaskkserv2 SKK server

[Service]
ExecStart=%h/.cargo/bin/yaskkserv2 --no-daemonize --listen-address 127.0.0.1:1178 %h/.skk/dictionary.yaskkserv2
Restart=always

[Install]
WantedBy=default.target
EOF

# サービス有効化
systemctl --user daemon-reload
systemctl --user enable yaskkserv2
log_success "yaskkserv2 systemd サービス設定完了"

log_info ""
log_info "※ yaskkserv2 を起動するには: systemctl --user start yaskkserv2"
log_info "※ fcitx5 設定は再ログイン後に fcitx5-configtool で行ってください"

# ============================================
# 完了メッセージ
# ============================================
echo ""
log_success "======================================"
log_success " セットアップ完了！"
log_success "======================================"
echo ""
log_info "以下のコマンドで環境変数を反映してください:"
log_info "  source ~/.bashrc"
echo ""
log_info "または、一度ログアウトして再ログインしてください。"
echo ""
log_info "--- インストール済みツール ---"
log_info "・基本パッケージ (build-essential, curl, wget, etc.)"
log_info "・Git + GitHub CLI"
log_info "・fzf, ripgrep, Starship"
log_info "・Neovim, Emacs"
log_info "・pyenv + Python"
log_info "・Volta + Node.js"
log_info "・Go"
log_info "・Java (Temurin JDK)"
log_info "・Clojure, Babashka, bbin, clojure-lsp, clj-kondo, cljfmt"
log_info "・fcitx5-skk + yaskkserv2"
echo ""
log_info "--- 追加設定が必要な項目 ---"
log_info "1. yaskkserv2 を起動: systemctl --user start yaskkserv2"
log_info "2. fcitx5 設定: 再ログイン後に fcitx5-configtool を実行"
log_info "3. Neovim プラグイン: nvim を起動して :Lazy sync"
log_info "4. Emacs 設定: 必要に応じて設定ファイルを配置"
echo ""
