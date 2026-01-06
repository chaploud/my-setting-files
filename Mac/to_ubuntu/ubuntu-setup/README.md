# Ubuntu 24.04 セットアップ自動化

Ansibleを使用したUbuntu 24.04開発環境の自動セットアップ。

## クイックスタート

### 1. 新規Ubuntuマシンで実行

```bash
# このリポジトリをクローン（またはコピー）
git clone <your-repo-url> ~/ubuntu-setup
cd ~/ubuntu-setup

# ブートストラップスクリプト実行（Ansibleインストール含む）
./bootstrap.sh
```

### 2. 個別に実行

```bash
# 特定のタグのみ実行
ansible-playbook playbook.yml --tags "base,terminal"

# ドライラン（変更を確認）
ansible-playbook playbook.yml --check

# 詳細出力
ansible-playbook playbook.yml -v
```

## 含まれるセットアップ

| タグ | 内容 |
|-----|------|
| `base` | 基本パッケージ、ビルドツール |
| `git` | Git設定、GitHub CLI |
| `terminal` | fzf, ripgrep, fd, bat, delta, Starship |
| `editors` | Neovim, Emacs |
| `python` | pyenv, Python |
| `nodejs` | Volta, Node.js |
| `go` | Go言語 |
| `java` | Temurin OpenJDK 21 |
| `clojure` | Clojure, Babashka, bbin, clojure-lsp, clj-kondo |
| `database` | PostgreSQL, SQLite |
| `docker` | Docker Engine, Docker Compose |
| `japanese` | yaskkserv2, ibus-skk |
| `utilities` | Ulauncher, Zeal, GNOME設定 |
| `dotfiles` | 設定ファイルのシンボリックリンク |

## カスタマイズ

`group_vars/all.yml` を編集してカスタマイズ:

```yaml
# ユーザー設定
user_name: "your-name"
user_email: "your@email.com"
github_user: "your-github-username"

# Pythonバージョン
python_version: "3.12.0"

# Node.jsバージョン
nodejs_version: "lts"

# dotfilesリポジトリ
dotfiles_repo: "https://github.com/your-username/dotfiles.git"
```

## ファイル構成

```
ubuntu-setup/
├── README.md
├── bootstrap.sh          # 初回実行用スクリプト
├── ansible.cfg           # Ansible設定
├── inventory.yml         # インベントリ（localhost）
├── playbook.yml          # メインPlaybook
└── group_vars/
    └── all.yml           # 変数定義
```

## 参考リンク

- [Ansible Documentation](https://docs.ansible.com/)
- [chezmoi](https://www.chezmoi.io/) - dotfiles管理（オプション）
- [TechDufus/dotfiles](https://github.com/techdufus/dotfiles) - 参考Ansible構成
