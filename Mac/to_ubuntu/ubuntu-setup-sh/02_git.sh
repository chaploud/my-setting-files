
# ============================================
# 2. Git & GitHub CLI
# ============================================
log_info "Git設定中..."

git config --global user.name "$GIT_USER_NAME"
git config --global user.email "$GIT_USER_EMAIL"
git config --global core.editor nvim
git config --global init.defaultBranch main

log_success "Git設定完了"

# GitHub CLI
if command_exists gh; then
    log_success "GitHub CLI は既にインストール済み"
else
    log_info "GitHub CLI をインストール中..."
    curl -fsSL https://cli.github.com/packages/githubcli-archive-keyring.gpg | sudo dd of=/usr/share/keyrings/githubcli-archive-keyring.gpg
    sudo chmod go+r /usr/share/keyrings/githubcli-archive-keyring.gpg
    echo "deb [arch=$(dpkg --print-architecture) signed-by=/usr/share/keyrings/githubcli-archive-keyring.gpg] https://cli.github.com/packages stable main" | sudo tee /etc/apt/sources.list.d/github-cli.list > /dev/null
    sudo apt update
    sudo apt install -y gh
    log_success "GitHub CLI インストール完了"
fi
