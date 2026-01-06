# Mac環境設定 - Windows/Ubuntu移行用リスト

## 1. プログラミング言語・ランタイム

### メイン言語
| 言語 | バージョン | バージョン管理 |
|------|-----------|---------------|
| Node.js | v25.1.0 | Volta |
| Python | 3.11, 3.13, 3.14 | pyenv |
| Ruby | 3.3.6 | rbenv |
| Go | 1.25.5 | - |
| Java | OpenJDK 21.0.8 (Temurin) | - |
| Rust | (cargo経由) | rustup |
| Clojure | 最新 | - |
| PHP | - | Homebrew |
| Lua (LuaJIT) | - | Homebrew |
| Zig | - | zls (Language Server) |

### バージョン管理ツール
- **pyenv** - Python
- **rbenv** + ruby-build - Ruby
- **Volta** - Node.js
- **fvm** - Flutter

---

## 2. Clojure エコシステム（特に充実）

| ツール | 用途 |
|--------|------|
| clojure | Clojure本体 |
| babashka | Clojureスクリプト実行 |
| clojure-lsp / clojure-lsp-native | Language Server |
| clj-kondo | Linter |
| cljfmt / cljstyle | フォーマッター |
| leiningen | ビルドツール |
| shadow-cljs | ClojureScript ビルド |
| calva (VSCode拡張) | IDE統合 |

---

## 3. エディタ・IDE

| ツール | 用途 |
|--------|------|
| Neovim | メインエディタ（Lazy.nvim管理） |
| Emacs (emacs-plus@30) | サブエディタ |
| VS Code | IDE |
| JetBrains Toolbox | JetBrains IDE管理 |

### Neovimプラグイン
- nvim-surround
- yanky.nvim
- im-select.nvim（IME自動切替）
- which-key.nvim

---

## 4. Language Servers

| LSP | 対象言語 |
|-----|---------|
| clojure-lsp | Clojure |
| jdtls | Java |
| solargraph | Ruby |
| terraform-ls | Terraform |
| vue-language-server | Vue.js |
| typescript-language-server | TypeScript |
| bash-language-server | Bash |
| yaml-language-server | YAML |
| vscode-langservers-extracted | HTML/CSS/JSON |
| zls | Zig |

---

## 5. データベース

| DB | バージョン | 状態 |
|----|-----------|------|
| PostgreSQL | 17 | 常時起動 |
| MySQL | 8.4 | 常時起動 |
| SQLite | - | インストール済 |

### 関連ツール
- pgAdmin4（PostgreSQL GUI）
- libpq, unixodbc

---

## 6. コンテナ・仮想化・クラウド

| ツール | バージョン/用途 |
|--------|----------------|
| Docker | 28.5.2 |
| OrbStack | Docker互換（macOS） |
| kubectl | v1.32.7 |
| kustomize | v5.5.0 |
| Helm | v3 |
| AWS CLI | awscli |
| Terraform | terraform-ls |

---

## 7. Git関連

### 設定
- **エディタ**: nvim
- **Pager**: delta（catppuccin-macchiatoテーマ）
- **署名**: SSH署名キー（ED25519）

### ツール
| ツール | 用途 |
|--------|------|
| gh | GitHub CLI |
| delta | diff表示 |
| fzf | ブランチ選択統合 |

### エイリアス（.zshrc）
```
gs = git status
ga = git add
gc = git commit
gp = git push
gd = git diff
gf = git pull origin $(current branch)
gff = git pull
gz = git stash -u
gzp = git stash pop
gr = git reset --hard HEAD
cdr = cd to git root
log = formatted git log
```

---

## 8. シェル設定

### 基本設定
| 項目 | 設定 |
|------|------|
| シェル | Zsh |
| プロンプト | Starship |
| 自動補完 | zsh-autosuggestions, zsh-completions |
| ディレクトリジャンプ | zoxide |
| 環境変数管理 | direnv |

### 主要エイリアス
```
# ls系
ls='ls -GF', ll='ls -lGF', sl/s/l=ls

# Docker
dcu = docker compose up
dcd = docker compose down
dcdv = docker compose down -v --remove-orphans

# Terraform
tf = terraform
```

### 環境変数・PATH
```
/opt/homebrew/bin
$HOME/.claude/local
$HOME/.volta/bin
$HOME/.cargo/bin
/opt/homebrew/opt/mysql@8.4/bin
/opt/homebrew/opt/postgresql@17/bin
$HOME/Library/Android/sdk/platform-tools
```

---

## 9. ターミナルツール

| ツール | 用途 |
|--------|------|
| Starship | プロンプト |
| fzf | ファジー検索 |
| ripgrep (rg) | 高速検索 |
| fd | 高速find |
| bat | catの代替（Catppuccin Macchiatoテーマ） |
| tree | ディレクトリ表示 |
| zellij | ターミナルマルチプレクサ |
| Ghostty | ターミナルエミュレータ |

---

## 10. セキュリティ・認証

| ツール | 用途 |
|--------|------|
| 1Password CLI | パスワード管理 |
| gnupg | GPG暗号化 |
| pass | パスワードストア |
| pinentry-mac | GPGピン入力 |

### SSH設定
- 1Password SSH Agent統合
- ED25519キーペア

---

## 11. マルチメディア処理

| ツール | 用途 |
|--------|------|
| ffmpeg | 動画処理 |
| ImageMagick | 画像処理 |
| GIMP | 画像編集GUI |
| libvips | 高速画像処理 |

### コーデック・ライブラリ
x264, x265, libvpx, libvorbis, opus, flac, dav1d, libwebp, libheif

---

## 12. フォント

| フォント | 用途 |
|----------|------|
| Hack Nerd Font | ターミナル |
| PlemolJP NF | 日本語プログラミング |
| SF Mono | macOSシステム |

---

## 13. 日本語入力

| ツール | 用途 |
|--------|------|
| macSKK | SKK日本語入力 |
| im-select.nvim | Neovim IME自動切替 |
| Karabiner-Elements | キーボードカスタマイズ |

---

## 14. ユーティリティ（macOS）

| ツール | 用途 |
|--------|------|
| Raycast | ランチャー |
| Rectangle | ウィンドウ管理 |
| Alt-Tab | ウィンドウ切替 |
| MiddleClick | トラックパッド中クリック |
| Dash | ドキュメント検索 |
| Proxyman | HTTPデバッグ |

---

## 15. ビルド・開発ライブラリ

### ビルドツール
cmake, make, autoconf, m4, pkg-config, libtool

### 圧縮
zlib, brotli, lz4, xz, zstd, snappy

### 暗号
openssl@3, libsodium

### その他
protobuf, pandoc, universal-ctags

---

## 16. WebAssembly

| ツール | 用途 |
|--------|------|
| wasmer | Wasmランタイム |
| wasmtime | Wasmランタイム |
| wasm-tools | Wasmツール |

---

## 17. npmグローバルパッケージ

| パッケージ | 用途 |
|-----------|------|
| prettier | コードフォーマッター |
| typescript | TypeScript |
| shadow-cljs | ClojureScript |
| @ionic/cli | Ionic CLI |

---

## 18. Rustパッケージ（cargo install）

| パッケージ | 用途 |
|-----------|------|
| cargo-web | Webビルド |
| emacs-lsp-booster | Emacs LSP高速化 |
| parinfer-rust | Parinfer |
| zellij | ターミナルマルチプレクサ |

---

## 19. Ruby Gems（グローバル）

### 主要フレームワーク
- Rails 8.0.2

### テスト
- RSpec関連（rspec-rails, rspec-core等）

### AWS SDK
- bedrock, dynamodb, s3, kms等多数

### Linter
- rubocop

---

## 20. 設定ファイルの場所

| 設定 | パス |
|------|------|
| Zsh設定 | ~/Documents/my-setting-files/Mac/zshrc |
| Zprofile | ~/Documents/my-setting-files/Mac/zprofile |
| Neovim | ~/Documents/my-setting-files/nvim/init.lua |
| Starship | ~/.config/starship.toml |
| Git | ~/.gitconfig |
| Karabiner | ~/.config/karabiner/karabiner.json |
| Bat | ~/.config/bat/config |
| Delta | ~/.config/delta/ |
| Ghostty | ~/.config/ghostty/ |
| Zellij | ~/.config/zellij/ |
| SSH | ~/.ssh/config |
| Ripgrep | ~/.ripgreprc |

---

## 21. Homebrew Taps（追加リポジトリ）

```
aquaproj/aqua
babashka/brew
borkdude/brew
clojure-lsp/brew
clojure/tools
d12frosted/emacs-plus
laishulu/homebrew
leoafarias/fvm
mtgto/macskk
oven-sh/bun
railwaycat/emacsmacport
sqldef/sqldef
```

---

## 22. サービス（自動起動）

| サービス | 状態 |
|----------|------|
| mysql@8.4 | 起動中 |
| postgresql@17 | 起動中 |

---

## 23. AI/開発支援ツール

| ツール | 用途 |
|--------|------|
| Claude Code | AI Coding Assistant |
| GitHub Copilot | AI補完 |
| codex-acp | Codex ACP |

---

## 統計サマリー

| カテゴリ | 数 |
|---------|-----|
| Homebrew Formula | 276個 |
| Homebrew Cask | 19個 |
| Homebrew Tap | 12個 |
| グローバルnpm | 21個 |
| Ruby Gems | 200+個 |
| Rust crates | 4個 |
