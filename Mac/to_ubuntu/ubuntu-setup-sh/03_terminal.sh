
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
