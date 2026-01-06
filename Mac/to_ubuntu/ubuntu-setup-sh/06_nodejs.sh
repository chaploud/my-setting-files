
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
