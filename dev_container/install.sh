#!/bin/bash
# 開発コンテナ内になるべく自分の環境を再現
# ホストPCでSSHキーエージェント転送していることを前提とする

# CLIツール
apt install -y tree jq ripgrep fzf neovim

# starship
curl -sS https://starship.rs/install.sh | sh -s -- --yes
mkdir -p ~/.config
cp ../starship/starship.toml ~/.config/starship.toml

# bashrc
echo ./bashrc >> ~/.bashrc

# VSCode拡張機能
if [ -f ./extensions.txt ]; then
  cat ./extensions.txt | xargs -L 1 code --install-extension
fi

# npxがあれば終了
if command -v npx &> /dev/null; then
  echo "npx is already installed."
else
  curl https://get.volta.sh | bash
  export VOLTA_HOME="$HOME/.volta"
  export PATH="$VOLTA_HOME/bin:$PATH"
  $VOLTA_HOME/bin/volta install node@22
fi

# claude code
npm install -g @anthropic-ai/claude-code