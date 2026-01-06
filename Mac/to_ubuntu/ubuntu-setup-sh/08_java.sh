
# ============================================
# 8. Java (Temurin OpenJDK)
# ============================================
if command_exists java; then
    log_success "Java は既にインストール済み: $(java -version 2>&1 | head -1)"
else
    log_info "Temurin JDK をインストール中..."

    # Adoptium GPG キー追加
    wget -qO - https://packages.adoptium.net/artifactory/api/gpg/key/public | sudo gpg --dearmor -o /usr/share/keyrings/adoptium.gpg

    # リポジトリ追加
    echo "deb [signed-by=/usr/share/keyrings/adoptium.gpg] https://packages.adoptium.net/artifactory/deb $(lsb_release -cs) main" | sudo tee /etc/apt/sources.list.d/adoptium.list

    sudo apt update

    # 最新LTS (21) をインストール
    sudo apt install -y temurin-21-jdk

    log_success "Temurin JDK インストール完了"
fi
