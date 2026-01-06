# Ubuntu 24.04 セットアップ手順

簡単な順に並べています。上から順に実行してください。

---

## 1. システム基本設定

```bash
# システムアップデート
sudo apt update && sudo apt upgrade -y

# 基本ビルドツール
sudo apt install -y build-essential curl wget git unzip zip

# Python/Ruby等のビルドに必要な依存関係
sudo apt install -y make libssl-dev zlib1g-dev \
  libbz2-dev libreadline-dev libsqlite3-dev llvm \
  libncurses5-dev libncursesw5-dev xz-utils tk-dev \
  libffi-dev liblzma-dev

# その他便利ツール
sudo apt install -y tree htop
```

---

## 2. Git

```bash
# 既にインストール済みのはずだが確認
git --version

# 設定
git config --global user.name "Your Name"
git config --global user.email "your.email@example.com"
git config --global core.editor nvim
git config --global init.defaultBranch main
```

---

## 3. GitHub CLI (gh)

```bash
# 公式リポジトリから最新版をインストール
curl -fsSL https://cli.github.com/packages/githubcli-archive-keyring.gpg | sudo dd of=/usr/share/keyrings/githubcli-archive-keyring.gpg
sudo chmod go+r /usr/share/keyrings/githubcli-archive-keyring.gpg
echo "deb [arch=$(dpkg --print-architecture) signed-by=/usr/share/keyrings/githubcli-archive-keyring.gpg] https://cli.github.com/packages stable main" | sudo tee /etc/apt/sources.list.d/github-cli.list > /dev/null
sudo apt update
sudo apt install -y gh

# 認証
gh auth login
```

---

## 4. ターミナルツール

### fzf

```bash
# aptでインストール
sudo apt install -y fzf

# または最新版をGitから
# git clone --depth 1 https://github.com/junegunn/fzf.git ~/.fzf
# ~/.fzf/install
```

### ripgrep (rg)

```bash
sudo apt install -y ripgrep
```

### fd

```bash
sudo apt install -y fd-find

# fdコマンドとして使うためのシンボリックリンク
mkdir -p ~/.local/bin
ln -s $(which fdfind) ~/.local/bin/fd
```

### bat

```bash
sudo apt install -y bat

# batコマンドとして使うためのシンボリックリンク
ln -s $(which batcat) ~/.local/bin/bat
```

### delta (git diff)

```bash
# Ubuntu 24.04ではSnapでインストール
sudo snap install git-delta-snap

# ~/.gitconfigに追加
git config --global core.pager "git-delta-snap.delta"
git config --global interactive.diffFilter "git-delta-snap.delta --color-only"
git config --global delta.line-numbers true
```

---

## 5. gnupg + pass

```bash
# GnuPG
sudo apt install -y gnupg

# passパスワードマネージャー
sudo apt install -y pass

# GPGキーの生成（未作成の場合）
# gpg --full-generate-key

# passの初期化（GPGキーIDを指定）
# pass init "your-gpg-key-id"
```

---

## 6. Starship (プロンプト)

```bash
# インストール
curl -sS https://starship.rs/install.sh | sh

# ~/.bashrcに追加
echo 'eval "$(starship init bash)"' >> ~/.bashrc

# 設定ファイルの配置（後でシンボリックリンク）
mkdir -p ~/.config
```

---

## 7. Neovim

```bash
# 最新版をaptで（PPA追加推奨）
sudo add-apt-repository ppa:neovim-ppa/unstable -y
sudo apt update
sudo apt install -y neovim

# または AppImage で最新版
# curl -LO https://github.com/neovim/neovim/releases/latest/download/nvim.appimage
# chmod u+x nvim.appimage
# sudo mv nvim.appimage /usr/local/bin/nvim
```

---

## 8. Emacs

```bash
# PPA経由でEmacs 30をインストール（推奨）
sudo add-apt-repository ppa:ubuntuhandbook1/emacs -y
sudo apt update
sudo apt install -y emacs

# または Snap で
# sudo snap install emacs --classic

# native-comp用の依存関係（ソースビルドの場合）
# sudo apt install -y libgccjit-12-dev
```

---

## 9. pyenv + Python

```bash
# pyenvインストール
curl https://pyenv.run | bash

# ~/.bashrcに追加
cat >> ~/.bashrc << 'EOF'
export PYENV_ROOT="$HOME/.pyenv"
[[ -d $PYENV_ROOT/bin ]] && export PATH="$PYENV_ROOT/bin:$PATH"
eval "$(pyenv init -)"
EOF

source ~/.bashrc

# Pythonインストール
pyenv install 3.12.0  # または希望のバージョン
pyenv global 3.12.0
```

---

## 10. Volta + Node.js

```bash
# Voltaインストール
curl https://get.volta.sh | bash

# シェルを再読み込み
source ~/.bashrc

# Node.jsインストール
volta install node@lts
```

---

## 11. Go

```bash
# 最新版をダウンロード（バージョンは適宜変更）
GO_VERSION="1.22.0"
wget https://go.dev/dl/go${GO_VERSION}.linux-amd64.tar.gz
sudo rm -rf /usr/local/go
sudo tar -C /usr/local -xzf go${GO_VERSION}.linux-amd64.tar.gz
rm go${GO_VERSION}.linux-amd64.tar.gz

# ~/.bashrcに追加
echo 'export PATH=$PATH:/usr/local/go/bin' >> ~/.bashrc
echo 'export PATH=$PATH:$HOME/go/bin' >> ~/.bashrc
source ~/.bashrc

# 確認
go version
```

---

## 12. Java (Temurin OpenJDK 21)

```bash
# Adoptium GPGキー追加
wget -qO - https://packages.adoptium.net/artifactory/api/gpg/key/public | sudo gpg --dearmor -o /usr/share/keyrings/adoptium.gpg

# リポジトリ追加
echo "deb [signed-by=/usr/share/keyrings/adoptium.gpg] https://packages.adoptium.net/artifactory/deb $(lsb_release -cs) main" | sudo tee /etc/apt/sources.list.d/adoptium.list

# インストール
sudo apt update
sudo apt install -y temurin-21-jdk

# 確認
java -version
```

---

## 13. Clojure エコシステム

### Clojure本体

```bash
# 依存関係（Javaは既にインストール済み）
sudo apt install -y rlwrap

# Clojure CLIインストール
curl -L -O https://github.com/clojure/brew-install/releases/latest/download/linux-install.sh
chmod +x linux-install.sh
sudo ./linux-install.sh
rm linux-install.sh

# 確認
clj --version
```

### Babashka

```bash
# インストールスクリプト
bash < <(curl -s https://raw.githubusercontent.com/babashka/babashka/master/install)

# 確認
bb --version
```

### bbin

```bash
# bbinインストール（babashkaが必要）
bash < <(curl -s https://raw.githubusercontent.com/babashka/bbin/main/install)

# ~/.bashrcにパス追加
echo 'export PATH="$HOME/.local/bin:$PATH"' >> ~/.bashrc
source ~/.bashrc
```

### clojure-lsp

```bash
# 最新版をダウンロード
curl -L -o /tmp/clojure-lsp.zip https://github.com/clojure-lsp/clojure-lsp/releases/latest/download/clojure-lsp-native-linux-amd64.zip
unzip /tmp/clojure-lsp.zip -d /tmp
sudo mv /tmp/clojure-lsp /usr/local/bin/
rm /tmp/clojure-lsp.zip

# 確認
clojure-lsp --version
```

### clj-kondo

```bash
# インストールスクリプト
curl -sLO https://raw.githubusercontent.com/clj-kondo/clj-kondo/master/script/install-clj-kondo
chmod +x install-clj-kondo
sudo ./install-clj-kondo
rm install-clj-kondo

# 確認
clj-kondo --version
```

### cljfmt

```bash
# bbinでインストール
bbin install io.github.weavejester/cljfmt
```

### shadow-cljs

```bash
# npmでグローバルインストール
npm install -g shadow-cljs
```

---

## 14. PostgreSQL

```bash
# インストール
sudo apt install -y postgresql postgresql-contrib

# サービス起動
sudo systemctl start postgresql
sudo systemctl enable postgresql

# ユーザー作成（オプション）
sudo -u postgres createuser --interactive
```

---

## 15. SQLite

```bash
# 通常プリインストール済み、なければ
sudo apt install -y sqlite3 libsqlite3-dev
```

---

## 16. Docker

```bash
# 古いバージョン削除
sudo apt remove -y docker docker-engine docker.io containerd runc 2>/dev/null || true

# 依存関係
sudo apt install -y ca-certificates curl gnupg

# Docker GPGキー追加
sudo install -m 0755 -d /etc/apt/keyrings
curl -fsSL https://download.docker.com/linux/ubuntu/gpg | sudo gpg --dearmor -o /etc/apt/keyrings/docker.gpg
sudo chmod a+r /etc/apt/keyrings/docker.gpg

# リポジトリ追加
echo \
  "deb [arch=$(dpkg --print-architecture) signed-by=/etc/apt/keyrings/docker.gpg] https://download.docker.com/linux/ubuntu \
  $(. /etc/os-release && echo "$VERSION_CODENAME") stable" | \
  sudo tee /etc/apt/sources.list.d/docker.list > /dev/null

# インストール
sudo apt update
sudo apt install -y docker-ce docker-ce-cli containerd.io docker-buildx-plugin docker-compose-plugin

# sudo無しで実行できるようにグループ追加
sudo usermod -aG docker $USER

# 再ログインまたは
newgrp docker

# 確認
docker --version
docker compose version
```

---

## 17. 日本語入力 (SKK)

### yaskkserv2

```bash
# Rustが必要（なければインストール）
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
source ~/.cargo/env

# yaskkserv2ビルド＆インストール
cargo install yaskkserv2

# SKK辞書ダウンロード
mkdir -p ~/.skk
cd ~/.skk
wget https://skk-dev.github.io/dict/SKK-JISYO.L.gz
gunzip SKK-JISYO.L.gz

# 辞書変換
yaskkserv2_make_dictionary --utf8 --dictionary-filename ~/.skk/dictionary.yaskkserv2 ~/.skk/SKK-JISYO.L

# systemdサービス作成
mkdir -p ~/.config/systemd/user
cat > ~/.config/systemd/user/yaskkserv2.service << 'EOF'
[Unit]
Description=yaskkserv2 SKK server

[Service]
ExecStart=%h/.cargo/bin/yaskkserv2 --no-daemonize --listen-address 127.0.0.1:1178 %h/.skk/dictionary.yaskkserv2
Restart=always

[Install]
WantedBy=default.target
EOF

# サービス有効化
systemctl --user enable yaskkserv2
systemctl --user start yaskkserv2
```

### ibus-skk

```bash
# インストール
sudo apt install -y ibus-skk

# IBus設定（GUIから設定）
# Settings > Keyboard > Input Sources > Other > Japanese (SKK)

# 設定が保存されない問題の対処（必要に応じて）
# https://github.com/k0kubun/ibus-skk をビルドしてインストール
```

**Note:** ibus-skkの設定保存問題がある場合は、[k0kubun/ibus-skk](https://github.com/k0kubun/ibus-skk)をビルドしてインストールする必要があります。

---

## 18. ユーティリティ（macOS代替）

### ランチャー（Raycast代替）

| 選択肢 | 特徴 | インストール |
|--------|------|-------------|
| **Ulauncher** | 軽量、プラグイン対応、Alfred風 | `sudo apt install ulauncher` |
| **Albert** | C++/Qt製、高速、プラグイン対応 | PPA経由 |
| **Rofi** | コマンド実行重視、スクリプト対応 | `sudo apt install rofi` |

```bash
# Ulauncher（推奨）
sudo add-apt-repository ppa:agornostal/ulauncher -y
sudo apt update
sudo apt install -y ulauncher

# 自動起動設定
# Settings > Startup Applications > Add > ulauncher --hide-window
```

### ウィンドウ管理（Rectangle代替）

**Ubuntu 24.04はデフォルトでタイル機能あり**

```bash
# Ubuntu Tiling Assistantが標準搭載
# Super + 矢印キーでウィンドウをタイル配置可能

# より高度な設定が必要な場合: Tiling Shell拡張
# GNOME Extensions から "Tiling Shell" をインストール
```

| 方法 | 説明 |
|------|------|
| **デフォルト** | Super+矢印でウィンドウスナップ（追加設定不要） |
| **Tiling Shell** | GNOME拡張、Windows 11風のスナップアシスト |
| **i3/Sway** | 本格的なタイル型WMに切り替え |

### ウィンドウ切替（Alt-Tab）

**GNOMEデフォルトで対応済み**

```bash
# ウィンドウ単位で切り替え（アプリ単位ではなく）
gsettings set org.gnome.desktop.wm.keybindings switch-windows "['<Alt>Tab']"

# 現在のワークスペースのみ表示
gsettings set org.gnome.shell.window-switcher current-workspace-only true
gsettings set org.gnome.shell.app-switcher current-workspace-only true
```

より高度なカスタマイズが必要な場合：
- **AATWS拡張**: [Advanced Alt-Tab Window Switcher](https://extensions.gnome.org/extension/4412/advanced-alttab-window-switcher/)

### 中クリック（MiddleClick代替）

**Ubuntu 24.04はデフォルトで3本指タップ=中クリック対応**

```bash
# タッチパッドのタップ設定確認
# Settings > Mouse & Touchpad > Tap to Click を有効化

# 2本指タップ=中クリック、3本指タップ=右クリックに変更したい場合
gsettings set org.gnome.desktop.peripherals.touchpad tap-button-map "lmr"

# デフォルトに戻す
gsettings reset org.gnome.desktop.peripherals.touchpad tap-button-map
```

### ドキュメント検索（Dash代替）

| 選択肢 | 特徴 | インストール |
|--------|------|-------------|
| **Zeal** | Dashと同じDocset使用、オフライン対応 | `sudo apt install zeal` |
| **DevDocs** | Web版、オフライン対応、無料 | https://devdocs.io |

```bash
# Zeal（推奨、Dashと互換性あり）
sudo apt install -y zeal

# 起動後: Tools > Docsets から必要なドキュメントをダウンロード
```

### 比較表

| macOS | Ubuntu | 備考 |
|-------|--------|------|
| Raycast | Ulauncher / Rofi | Ulauncher推奨 |
| Rectangle | デフォルト / Tiling Shell | 標準で対応 |
| Alt-Tab | デフォルト | gsettingsでカスタマイズ可 |
| MiddleClick | デフォルト | 3本指タップで対応 |
| Dash | Zeal | Docset互換 |
| Karabiner | xmodmap / keyd | キーボード設定は別途 |

---

## 19. 設定ファイルの配置

```bash
# dotfilesリポジトリをクローン（あれば）
# git clone https://github.com/your-username/dotfiles.git ~/dotfiles

# シンボリックリンク作成例
ln -sf ~/dotfiles/bashrc ~/.bashrc
ln -sf ~/dotfiles/starship.toml ~/.config/starship.toml
ln -sf ~/dotfiles/nvim ~/.config/nvim
ln -sf ~/dotfiles/.gitconfig ~/.gitconfig

# Emacs設定
ln -sf ~/dotfiles/.emacs.d ~/.emacs.d
# または
# git clone https://github.com/your-username/emacs.d.git ~/.emacs.d
```

---

## 20. 最終確認

```bash
# バージョン確認
git --version
gh --version
nvim --version
emacs --version
python --version
node --version
go version
java -version
clj --version
bb --version
docker --version
psql --version
```

---

## 参考リンク

### 言語・ツール
- [pyenv on Ubuntu](https://dev.to/emdadul38/how-to-install-pyenv-on-ubuntu-2404-5807)
- [Emacs 30 PPA](https://ubuntuhandbook.org/index.php/2023/08/gnu-emacs-29-1-ubuntu-ppa/)
- [Volta](https://volta.sh/)
- [babashka](https://github.com/babashka/babashka)
- [bbin](https://github.com/babashka/bbin)
- [Docker公式ドキュメント](https://docs.docker.com/engine/install/ubuntu/)

### 日本語入力
- [yaskkserv2](https://github.com/wachikun/yaskkserv2)
- [ibus-skk設定問題](https://qiita.com/k0kubun/items/d435dcffea73bfb092e5)
- [k0kubun/ibus-skk](https://github.com/k0kubun/ibus-skk)

### ユーティリティ
- [Ulauncher](https://ulauncher.io/)
- [Rofi](https://github.com/davatorium/rofi)
- [Ubuntu Tiling Windows](https://itsfoss.com/ubuntu-tiling-windows/)
- [Tiling Shell](https://www.omgubuntu.co.uk/2024/06/tiling-shell-gnome-extension-in-ubuntu)
- [AATWS (Alt-Tab拡張)](https://github.com/G-dH/advanced-alttab-window-switcher)
- [Zeal (Dash代替)](https://zealdocs.org/)
- [タッチパッド設定](https://ubuntuhandbook.org/index.php/2025/07/swap-two-and-three-finger-tap-ubuntu-24-04/)
