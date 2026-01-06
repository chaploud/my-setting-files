
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
