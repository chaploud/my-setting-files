
# ============================================
# 9. Clojure エコシステム
# ============================================

# Clojure CLI
if command_exists clj; then
    log_success "Clojure は既にインストール済み"
else
    log_info "Clojure をインストール中..."
    curl -L -O https://github.com/clojure/brew-install/releases/latest/download/linux-install.sh
    chmod +x linux-install.sh
    sudo ./linux-install.sh
    rm linux-install.sh
    log_success "Clojure インストール完了"
fi

# Babashka
if command_exists bb; then
    log_success "Babashka は既にインストール済み"
else
    log_info "Babashka をインストール中..."
    curl -sLO https://raw.githubusercontent.com/babashka/babashka/master/install
    chmod +x install
    sudo ./install
    rm install
    log_success "Babashka インストール完了"
fi

# bbin
if [[ -f "$HOME/.local/bin/bbin" ]]; then
    log_success "bbin は既にインストール済み"
else
    log_info "bbin をインストール中..."
    curl -o- -L https://raw.githubusercontent.com/babashka/bbin/main/install | bash
    log_success "bbin インストール完了"
fi

# clojure-lsp
if command_exists clojure-lsp; then
    log_success "clojure-lsp は既にインストール済み"
else
    log_info "clojure-lsp をインストール中..."
    curl -L -o /tmp/clojure-lsp.zip https://github.com/clojure-lsp/clojure-lsp/releases/latest/download/clojure-lsp-native-linux-amd64.zip
    unzip -o /tmp/clojure-lsp.zip -d /tmp
    sudo mv /tmp/clojure-lsp /usr/local/bin/
    rm /tmp/clojure-lsp.zip
    log_success "clojure-lsp インストール完了"
fi

# clj-kondo
if command_exists clj-kondo; then
    log_success "clj-kondo は既にインストール済み"
else
    log_info "clj-kondo をインストール中..."
    curl -sLO https://raw.githubusercontent.com/clj-kondo/clj-kondo/master/script/install-clj-kondo
    chmod +x install-clj-kondo
    sudo ./install-clj-kondo
    rm install-clj-kondo
    log_success "clj-kondo インストール完了"
fi

# cljfmt (bbin経由)
if [[ -f "$HOME/.local/bin/cljfmt" ]]; then
    log_success "cljfmt は既にインストール済み"
else
    log_info "cljfmt をインストール中..."
    export PATH="$HOME/.local/bin:$PATH"
    if command_exists bbin; then
        bbin install io.github.weavejester/cljfmt
        log_success "cljfmt インストール完了"
    else
        log_warn "bbin が見つかりません。cljfmt をスキップします"
    fi
fi
