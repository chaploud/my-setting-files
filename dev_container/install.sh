#!/bin/bash
# 開発コンテナ内になるべく自分の環境を再現

# CLIツール
apt install -y tree jq ripgrep fzf neovim

# starship
curl -sS https://starship.rs/install.sh | sh -s -- --yes
mkdir -p ~/.config
cp ../starship/starship.toml ~/.config/starship.toml

# bashrc
cp ./bashrc ~/.bashrc

# VSCode拡張機能
if [ -f ./extensions.txt ]; then
  cat ./extensions.txt | xargs -L 1 code --install-extension
fi