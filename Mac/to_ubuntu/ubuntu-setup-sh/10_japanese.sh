
# ============================================
# 10. 日本語入力 (fcitx5-skk + yaskkserv2)
# ============================================

# fcitx5-skk
log_info "fcitx5-skk をインストール中..."
sudo apt install -y fcitx5 fcitx5-skk im-config

log_info "fcitx5 を既定の入力メソッドに設定中..."
im-config -n fcitx5

# 環境変数を設定
if ! grep -q 'GTK_IM_MODULE=fcitx' "$HOME/.bashrc"; then
    cat >> "$HOME/.bashrc" << 'EOF'

# fcitx5
export GTK_IM_MODULE=fcitx
export QT_IM_MODULE=fcitx
export XMODIFIERS=@im=fcitx
EOF
    log_info "fcitx5 環境変数を .bashrc に追加"
fi

log_success "fcitx5-skk インストール完了"

# Rust (yaskkserv2 用)
if [[ -f "$HOME/.cargo/bin/rustc" ]]; then
    log_success "Rust は既にインストール済み"
else
    log_info "Rust をインストール中..."
    curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- -y
    log_success "Rust インストール完了"
fi

# Cargo 環境読み込み
if [[ -f "$HOME/.cargo/env" ]]; then
    source "$HOME/.cargo/env"
fi

# yaskkserv2
if [[ -f "$HOME/.cargo/bin/yaskkserv2" ]]; then
    log_success "yaskkserv2 は既にインストール済み"
else
    log_info "yaskkserv2 をインストール中..."
    cargo install yaskkserv2
    log_success "yaskkserv2 インストール完了"
fi

# SKK辞書
mkdir -p "$HOME/.skk"
if [[ ! -f "$HOME/.skk/SKK-JISYO.L" ]]; then
    log_info "SKK辞書をダウンロード中..."
    wget -q https://skk-dev.github.io/dict/SKK-JISYO.L.gz -O "$HOME/.skk/SKK-JISYO.L.gz"
    gunzip -f "$HOME/.skk/SKK-JISYO.L.gz"
    log_success "SKK辞書ダウンロード完了"
fi

# yaskkserv2 辞書ビルド
if [[ ! -f "$HOME/.skk/dictionary.yaskkserv2" ]]; then
    log_info "yaskkserv2 辞書をビルド中..."
    "$HOME/.cargo/bin/yaskkserv2_make_dictionary" \
        --utf8 \
        --dictionary-filename "$HOME/.skk/dictionary.yaskkserv2" \
        "$HOME/.skk/SKK-JISYO.L"
    log_success "yaskkserv2 辞書ビルド完了"
fi

# systemd ユーザーサービス作成
mkdir -p "$HOME/.config/systemd/user"
cat > "$HOME/.config/systemd/user/yaskkserv2.service" << 'EOF'
[Unit]
Description=yaskkserv2 SKK server

[Service]
ExecStart=%h/.cargo/bin/yaskkserv2 --no-daemonize --listen-address 127.0.0.1:1178 %h/.skk/dictionary.yaskkserv2
Restart=always

[Install]
WantedBy=default.target
EOF

# サービス有効化
systemctl --user daemon-reload
systemctl --user enable yaskkserv2
log_success "yaskkserv2 systemd サービス設定完了"

log_info ""
log_info "※ yaskkserv2 を起動するには: systemctl --user start yaskkserv2"
log_info "※ fcitx5 設定は再ログイン後に fcitx5-configtool で行ってください"
