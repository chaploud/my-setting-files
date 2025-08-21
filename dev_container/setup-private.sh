#!/bin/bash
# setup-private.sh - personal setup script for container

SERVICE_TYPE=$1

setup_tools() {
    echo "[Private Setup]..."
    # 任意のコマンドをdocker compose up時に実行できる
    apt update
    apt install -y tree jq ripgrep fzf neovim unzip inotify-tools
    if ! command -v starship > /dev/null 2>&1; then
        curl -sS https://starship.rs/install.sh | sh -s -- --yes
    fi
    # clojure-lspをコンテナ内で使いたいなら
    if ! command -v clojure-lsp > /dev/null 2>&1; then
        bash < <(curl -s https://raw.githubusercontent.com/clojure-lsp/clojure-lsp/master/install)
    fi
}

# serverサービス用のNode.jsセットアップ
setup_nodejs_for_server() {
    if ! command -v node > /dev/null 2>&1; then
        echo "[Claude Setup] Installing Node.js..."
        curl -fsSL https://deb.nodesource.com/setup_22.x -o /tmp/nodesource_setup.sh
        bash /tmp/nodesource_setup.sh
        apt update
        apt install -y nodejs
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

# 個人用設定のベタ書き
setup_starship() {
     mkdir -p "$HOME/.config"
     cat > "$HOME/.config/starship.toml" <<'EOF'
continuation_prompt = '[❯❯ ](fg:blue)'

[git_status]
format = '$all_status $ahead_behind '
diverged = '[￪${ahead_count}](cyan)/[￬${behind_count}](red)'
conflicted = '[=${count}](red)'
ahead = '[￪${count}](cyan)'
behind = '[￬${count}](red)'
untracked = '[?${count}](cyan)'
stashed = '[#${count}](blue)'
modified = '[!${count}](yellow)'
staged = '[+${count}](green)'
renamed = '[r${count}](blue)'
deleted = '[x${count}](red)'

[directory]
format = '[$path](bold blue)[$read_only]($read_only_style) '
truncate_to_repo = false
truncation_symbol = "../"

[git_branch]
format = '[$branch(:$remote_branch)](bold purple) '

[cmd_duration]
disabled = true

[lua]
disabled = true

[nodejs]
disabled = true

[package]
disabled = true

[java]
disabled = true

[python]
disabled = true

[rust]
disabled = true

[dart]
disabled = true

[ruby]
disabled = true

[golang]
disabled = true

[hostname]
disabled = true

[username]
show_always = true
format = '[$user](bold green) '

[container]
disabled = true
EOF
}

setup_bashrc() {
    local marker="### Private Setup Begin ###"
    if ! grep -qF "$marker" "$HOME/.bashrc"; then
        cat >> "$HOME/.bashrc" <<'EOF'

### Private Setup Begin ###
# ctrl+wを/まで削除の挙動に
stty werase undef
bind \\C-w:unix-filename-rubout

# ls
alias sl="ls"
alias s="ls"

# Git
alias log="git log --decorate --format=format:'%C(bold cyan)%as%C(reset) %C(bold blue)%h%C(reset) %C(white bold)%<(14,trunc)%an%C(reset) %C(green)%<(38,trunc)%s%C(reset) '"
alias gs="git status"
alias ga="git add"
alias gc="git commit"
alias gp="git push"
alias gl=log
alias gd="git diff"
alias gf='git pull origin $(git branch --show-current)'
alias gff='git pull'
alias gz='git stash -u'
alias gzp='git stash pop'

# cd後にlsを実行
function cdls() {
  \cd $@ && ls
}
alias cd="cdls"

# === Starship
eval "$(starship init bash)"

if [ -d "/usr/local/eboshigara" ]; then
  cd /usr/local/eboshigara
elif [ -d "/usr/local/mukudori" ]; then
  cd /usr/local/mukudori
fi

### Private Setup End ###
EOF
    fi
}

# ===== メイン処理
setup_tools
setup_starship
setup_bashrc

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
