#!/bin/bash
# 開発コンテナ内になるべく自分の環境を再現
# ホストPCでSSHキーエージェント転送していることを前提とする
# 個人用拡張期機能はsettings.jsonに記載

DOT=$HOME/dotfiles
DEV=$DOT/dev_container

# CLIツール
apt install -y tree jq ripgrep fzf neovim

# starship
curl -sS https://starship.rs/install.sh | sh -s -- --yes
mkdir -p $HOME/.config
cp $DOT/starship/starship.toml $HOME/.config/starship.toml

# bashrc
cat $DEV/bashrc >> $HOME/.bashrc

# claude code
npm install -g @anthropic-ai/claude-code
echo 'export ANTHROPIC_API_KEY=$(op read "op://Private/Claude Code API Key/credential")' >> $HOME/.bashrc