#!/bin/bash
# 開発コンテナ内になるべく自分の環境を再現
# ホストPCでSSHキーエージェント転送していることを前提とする
# 個人用拡張期機能はsettings.jsonに記載

set -ex # エラー時停止 & コマンド実行ログ表示

DOT=$HOME/dotfiles
DEV=$DOT/dev_container

echo "=== Dev Container Setup Started ==="
echo "DOT: $DOT"
echo "DEV: $DEV"
echo "PWD: $(pwd)"
echo "USER: $(whoami)"
echo "HOME: $HOME"

# CLIツール
apt install -y tree jq ripgrep fzf neovim

# starship
curl -sS https://starship.rs/install.sh | sh -s -- --yes
mkdir -p $HOME/.config
cp $DOT/starship/starship.toml $HOME/.config/starship.toml

# bashrc
echo $DEV/bashrc >> $HOME/.bashrc

if command -v npm &> /dev/null; then
  echo "npm is already installed."
else
  curl https://get.volta.sh | bash
  export VOLTA_HOME="$HOME/.volta"
  export PATH="$VOLTA_HOME/bin:$PATH"
  $VOLTA_HOME/bin/volta install node@22
fi

# claude code
npm install -g @anthropic-ai/claude-code