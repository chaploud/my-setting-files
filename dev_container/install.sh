#!/bin/bash
# 開発コンテナ内になるべく自分の環境を再現
# ホストPCでSSHキーエージェント転送していることを前提とする

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

# VSCode拡張機能
echo "=== Installing VSCode Extensions ==="
# codeコマンドが利用可能になるまで最大30秒待機
for i in {1..6}; do
  if command -v code &> /dev/null; then
    echo "code command is available, proceeding with extension installation"
    break
  else
    echo "Waiting for code command to be available... (attempt $i/6)"
    sleep 5
  fi
done

if command -v code &> /dev/null; then
  if [ -f $DEV/extensions.txt ]; then
    echo "Found extensions.txt, installing..."
    cat $DEV/extensions.txt | xargs -L 1 code --install-extension
  else
    echo "extensions.txt not found at $DEV/extensions.txt"
  fi
else
  echo "code command not available after 30 seconds, skipping extension installation"
fi