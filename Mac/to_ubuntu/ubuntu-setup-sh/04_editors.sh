
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
