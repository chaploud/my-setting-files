# Emacs設定リニューアル計画

## 現状分析

### ファイル構成
- `early-init.el`: 65行
- `init.el`: 約2300行（肥大化）

### 現在の設定に含まれる要素

| カテゴリ | 内容 |
|---------|------|
| 基盤 | package.el, use-package |
| 環境 | exec-path-from-shell, auth-source |
| 日本語 | ddskk, macism, ccc |
| UI | catppuccin-theme, doom-modeline, dashboard, nerd-icons |
| 補完 | vertico, orderless, marginalia, consult, embark, corfu, cape |
| Evil | evil + 10個の拡張パッケージ |
| ワークスペース | perspective.el |
| ファイルツリー | treemacs + 拡張 |
| Git | magit, forge, diff-hl |
| ターミナル | vterm |
| AI | copilot, claude-code-ide |
| LSP | eglot, tempel |
| 言語 | Clojure, Shell, C, JS/TS, Python, Ruby, Rust, Zig 等 |
| キーバインド | general.el |
| HTTP | restclient, plz.el |
| フィード | elfeed |

---

## リニューアル方針

### 1. 宣言的に
- `use-package` の `:custom`, `:hook`, `:bind` を活用
- グローバル変数の直接設定を避ける

### 2. 適切な分割
```
emacs.d.new/
├── early-init.el     # GUI/GC/ネイティブコンパイル
├── init.el           # ブートストラップのみ
└── lisp/
    ├── init-core.el      # Emacs基本設定
    ├── init-ui.el        # 外観・テーマ
    ├── init-completion.el # 補完システム
    ├── init-evil.el      # Vimエミュレーション
    ├── init-git.el       # Git連携
    ├── init-lsp.el       # LSP/eglot
    ├── init-lang-*.el    # 言語別設定
    └── init-keybinds.el  # キーバインド
```

### 3. デファクト確認（2025年12月時点）
- パッケージ管理: `use-package` (組み込み) + `package.el`
- 補完: vertico/corfu/consult スタック
- LSP: eglot (組み込み)
- Vim: evil
- テーマ: catppuccin等

### 4. 削除候補
- [ ] 使っていないユーティリティ関数
- [ ] 重複する設定
- [ ] 不要な言語サポート

---

## 進行ステップ

### Phase 1: 基盤（最小起動可能な設定）
1. [ ] early-init.el
2. [ ] init.el（パッケージ管理のみ）
3. [ ] init-core.el（Emacs基本設定）

### Phase 2: UI/操作
4. [ ] init-ui.el（テーマ、モードライン、フォント）
5. [ ] init-completion.el（vertico/corfu/consult）
6. [ ] init-evil.el

### Phase 3: 開発環境
7. [ ] init-git.el
8. [ ] init-lsp.el
9. [ ] 各言語設定

### Phase 4: 追加機能
10. [ ] init-keybinds.el
11. [ ] AI連携（copilot, claude-code-ide）
12. [ ] その他（vterm, elfeed等）

---

## 次のアクション

**Phase 1-1: early-init.el から始めましょう**

確認事項:
1. Emacs 30.x を使用している前提でOK？
2. ネイティブコンパイルは必須？
3. カラーテーマは catppuccin を継続？
