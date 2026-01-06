
# ============================================
# 5. Python (pyenv)
# ============================================
if [[ -d "$HOME/.pyenv" ]]; then
    log_success "pyenv は既にインストール済み"
else
    log_info "pyenv をインストール中..."
    curl https://pyenv.run | bash
    log_success "pyenv インストール完了"
fi

# bashrc に pyenv 追加
if ! grep -q 'PYENV_ROOT' "$HOME/.bashrc"; then
    cat >> "$HOME/.bashrc" << 'EOF'

# pyenv
export PYENV_ROOT="$HOME/.pyenv"
[[ -d $PYENV_ROOT/bin ]] && export PATH="$PYENV_ROOT/bin:$PATH"
eval "$(pyenv init -)"
EOF
    log_info "pyenv を .bashrc に追加"
fi

# Python インストール
export PYENV_ROOT="$HOME/.pyenv"
export PATH="$PYENV_ROOT/bin:$PATH"
if [[ -d "$PYENV_ROOT" ]]; then
    eval "$(pyenv init -)"
    if [[ ! -d "$PYENV_ROOT/versions/$PYTHON_VERSION" ]]; then
        log_info "Python $PYTHON_VERSION をインストール中..."
        pyenv install "$PYTHON_VERSION"
        pyenv global "$PYTHON_VERSION"
        log_success "Python $PYTHON_VERSION インストール完了"
    else
        log_success "Python $PYTHON_VERSION は既にインストール済み"
    fi
fi
