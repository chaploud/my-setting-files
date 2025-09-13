# SpaceMacsとDoom Emacsの包括的なキーバインド一覧

この包括的なガイドは、一般的な開発作業向けのSpaceMacsとDoom Emacsキーバインドの詳細な比較を提供し、SPCキー（スペース）とカンマをトリガーとする言語非依存の機能に焦点を当てています。

## コアリーダーキーの違い

基本的なキーバインドアーキテクチャは、2つのディストリビューション間で若干異なります：

| キー | SpaceMacs | Doom Emacs |
|-----|-----------|------------|
| **プライマリリーダー** | SPC (space) | SPC (space) |
| **メジャーモードリーダー** | , (shortcut for SPC m) | , (local leader) |
| **セカンダリリーダー** | M-m (in Emacs style) | 使用されない |
| **コマンドパレット** | SPC SPC (M-x) | SPC : (M-x) |
| **クイックファイル検索** | SPC f f (helm/ivy) | SPC SPC (projectile) |

## 1. ファイル操作

ファイル管理のキーバインドはほとんどの機能を共有していますが、実装の詳細と一部の特定の操作で違いがあります。

| キーバインド | SpaceMacs | Doom Emacs | 説明 |
|------------|-----------|------------|-------------|
| **SPC f f** | `spacemacs/helm-find-files` | `projectile-find-file` | ファイルの検索/開く（Doomではプロジェクト対応） |
| **SPC f s** | `save-buffer` | `save-buffer` | 現在のファイルを保存 |
| **SPC f S** | Save all files | `save-some-buffers` | 変更されたすべてのファイルを保存 |
| **SPC f r** | Recent files | `counsel-recentf` | 最近使用したファイルを開く |
| **SPC f R** | Rename current file | `doom/move-this-file` | 現在のファイルをリネーム/移動 |
| **SPC f D** | Delete current file | `doom/delete-this-file` | 現在のファイルを削除 |
| **SPC f C** | Copy file | `doom/copy-this-file` | 現在のファイルをコピー |
| **SPC f t** | Toggle file tree | デフォルトなし | ファイルツリーの表示/非表示 |
| **SPC f y** | Show/copy file path | デフォルトなし | ファイルパスのコピー |
| **SPC f e d** | Open .spacemacs | 該当なし | 設定ファイルを編集 |
| **SPC f p** | 該当なし | `doom/find-file-in-private-config` | Doom設定を開く |
| **SPC .** | デフォルトなし | `find-file` | ファイルを参照（Doom） |
| **SPC o p** | デフォルトなし | Toggle file tree | treemacsの表示/非表示（Doom） |

SpaceMacsは**SPC f e**で設定ファイル編集のより記憶に残るファイル操作を提供し、Doomはプロジェクト対応のファイル検索をより深く統合しています。ファイルツリーの切り替えは場所が異なり、Doomでは組織ツール用の**SPC o p**に配置されています。

## 2. 検索機能

検索操作は両方のディストリビューションの重要な部分であり、デフォルトの検索ツールとキーバインドの構成に若干の違いがあります。

| キーバインド | SpaceMacs | Doom Emacs | 説明 |
|------------|-----------|------------|-------------|
| **SPC s s** | Search in file | `swiper` | 現在のバッファで検索 |
| **SPC s S** | Search with default input | `swiper-thing-at-point` | カーソル下の文字列を検索 |
| **SPC /** | Search in project | Search in project | プロジェクト全体の検索 |
| **SPC s p** | Search in project | `+default/search-project` | プロジェクトでツールを使用した検索 |
| **SPC s P** | Search with default input | `+default/search-other-project` | 他のプロジェクトで検索 |
| **SPC s b** | Search in buffers | `swiper-all` | 全バッファを横断検索 |
| **SPC s d** | Search in directory | `+default/search-cwd` | 現在のディレクトリで検索 |
| **SPC s i** | デフォルトなし | `imenu` | バッファ内のシンボルを検索 |
| **SPC s j** | Jump to symbol | デフォルトなし | シンボルにジャンプ |
| **SPC s a p** | Search with ag | デフォルトなし | agでプロジェクト検索 |
| **SPC s r p** | Search with ripgrep | デフォルトなし | ripgrepでプロジェクト検索 |
| **SPC s o** | デフォルトなし | `+lookup/online` | オンライン検索 |

両方のディストリビューションは複数の検索バックエンド（ag、ripgrep、grep）をサポートしていますが、Doomは`+default/search-*`関数を通じてより統一されたラッパーを提供します。SpaceMacsはag用の**SPC s a**やripgrep用の**SPC s r**など、特定のプレフィックスを使用した検索ツール選択をより細かく制御できます。

## 3. Git操作

バージョン管理統合は両方のディストリビューションでMagitを中心とし、ほぼ同じキーバインドですが、一部の命名の違いがあります。

| キーバインド | SpaceMacs | Doom Emacs | 説明 |
|------------|-----------|------------|-------------|
| **SPC g s** | `magit-status` | デフォルトなし(SPC g g参照) | Gitステータス（SpaceMacs） |
| **SPC g g** | デフォルトなし | `magit-status` | Gitステータス（Doom推奨） |
| **SPC g S** | Stage current file | `magit-stage-file` | 現在のファイルをステージング |
| **SPC g U** | Unstage current file | `magit-unstage-file` | 現在のファイルのステージングを解除 |
| **SPC g c** | Commit changes | `magit-commit` | コミット作成 |
| **SPC g C** | Checkout branches | `magit-checkout` | ブランチ/コミットをチェックアウト |
| **SPC g b** | Git blame | `magit-blame` | git blameを表示 |
| **SPC g B** | Quit git blame | `magit-blame-quit` | blameモードを終了 |
| **SPC g l** | Git log | `magit-log` | gitログを表示 |
| **SPC g L** | File log | `magit-log-buffer-file` | 現在のファイルのログ |
| **SPC g d** | Show diff | `magit-diff` | diff表示プロンプト |
| **SPC g D** | Diff against HEAD | `magit-diff-head` | HEADとのdiff |
| **SPC g t** | Git time machine | `git-timemachine` | ファイル履歴の閲覧 |
| **SPC g P** | Push prompt | `magit-push-popup` | オプション付きでpush |
| **SPC g F** | Pull/fetch prompt | `magit-pull` | リモートからpull |

主な違いは、メインのstatusコマンドにあります：SpaceMacsは**SPC g s**を使用し、Doomは**SPC g g**を好みます。両方ともカスタマイズ可能ですが、これらがデフォルトです。

## 4. ナビゲーションとジャンプ

コードナビゲーションは、avyと言語サーバープロトコル統合を通じた高度なジャンプ機能とevil-modeの移動を組み合わせています。

| キーバインド | SpaceMacs | Doom Emacs | 説明 |
|------------|-----------|------------|-------------|
| **SPC SPC** | `avy-goto-char-timer` | `projectile-find-file` | クイックジャンプ/検索（異なる） |
| **SPC j j** | Jump to character | `evil-avy-goto-char-timer` | 文字にジャンプ |
| **SPC j l** | Jump to line | `evil-avy-goto-line` | 行にジャンプ |
| **SPC j w** | Jump to word | `evil-avy-goto-word-1` | 単語にジャンプ |
| **SPC j f** | Jump to function | `find-function` | 関数定義にジャンプ |
| **SPC j v** | Jump to variable | `find-variable` | 変数にジャンプ |
| **SPC j i** | Jump to symbol | デフォルトなし | imenuを使用してジャンプ |
| **SPC j d** | デフォルトなし | `+lookup/definition` | 定義にジャンプ |
| **SPC c d** | 該当なし | `+lookup/definition` | コード定義（Doom） |
| **SPC c D** | 該当なし | `+lookup/references` | 参照を検索（Doom） |
| **g d** | Go to definition | `+lookup/definition` | 定義への高速ジャンプ |

DoomはLSP対応ナビゲーションをコード操作用の**SPC c**プレフィックスでより目立つように統合し、SpaceMacsはナビゲーションを主に**SPC j**下に保持します。**SPC SPC**バインドは大きく異なります：SpaceMacsは文字ジャンプに使用し、Doomはファイル検索に使用します。

## 5. ターミナルとシェル操作

ターミナル統合は、ターミナルエミュレーターの選択とモジュール設定に基づいてディストリビューション間で異なります。

| キーバインド | SpaceMacs | Doom Emacs | 説明 |
|------------|-----------|------------|-------------|
| **SPC '** | Open shell popup | デフォルトなし | ポップアップでシェル（SpaceMacs） |
| **SPC o t** | デフォルトなし | `+vterm/toggle` | ターミナルの表示/非表示（Doom） |
| **SPC o T** | デフォルトなし | `+vterm/here` | 現在のディレクトリでターミナル |
| **SPC o e** | デフォルトなし | `+eshell/toggle` | eshellの表示/非表示 |
| **SPC p '** | Shell in project root | デフォルトなし | プロジェクトルートでシェル |
| **SPC a s** | Shell applications menu | デフォルトなし | シェルメニュー（SpaceMacs） |
| **SPC !** | デフォルトなし | `shell-command` | シェルコマンドを実行 |

SpaceMacsは**SPC '**を主要なシェルアクセスポイントとして使用し、Doomはターミナル操作を**SPC o**（開く/組織ツール）下に整理します。Doomのターミナルサポートには`:term vterm`や`:term eshell`などの特定のモジュールを有効にする必要があります。

## 6. バッファとウィンドウ管理

バッファとウィンドウ操作は、共有されたevil-modeの遺産を反映して、ディストリビューション間で大部分が一貫しています。

| キーバインド | SpaceMacs | Doom Emacs | 説明 |
|------------|-----------|------------|-------------|
| **SPC b b** | `helm-mini` | `persp-switch-to-buffer` | バッファ切り替え |
| **SPC b B** | Switch to buffer (ibuffer) | `switch-to-buffer` | 代替バッファ切り替え |
| **SPC TAB** | Previous buffer | Previous workspace/buffer | 前回への高速切り替え |
| **SPC b d** | Delete buffer | `kill-current-buffer` | 現在のバッファを削除 |
| **SPC b k** | Kill buffer | `kill-current-buffer` | バッファを削除（エイリアス） |
| **SPC b s** | Save buffer | `save-buffer` | 現在のバッファを保存 |
| **SPC b n** | Next buffer | `next-buffer` | 次のバッファに循環 |
| **SPC b p** | Previous buffer | `previous-buffer` | 前のバッファに循環 |
| **SPC b h** | Home buffer | デフォルトなし | SpaceMacsスタート画面 |
| **SPC w v** | Split vertically | `evil-window-vsplit` | 垂直ウィンドウ分割 |
| **SPC w s** | Split window | `evil-window-split` | 水平分割 |
| **SPC w d** | Delete window | `evil-quit` | 現在のウィンドウを閉じる |
| **SPC w w** | Other window | `other-window` | ウィンドウを切り替え |
| **SPC w m** | Maximize window | `doom/window-maximize-buffer` | 現在のウィンドウを最大化 |
| **SPC w h/j/k/l** | Move to window | Move to window | 方向別ナビゲーション |
| **SPC 1-9** | Switch to window 1-9 | デフォルトなし | 番号付きウィンドウ切り替え |

ウィンドウ管理はほぼ同じままで、バッファ操作は実装（helm vs ivy/consult）とワークスペース統合で若干異なります。

## 7. プロジェクト管理

プロジェクト操作は両方のディストリビューションで一貫したキーバインドを持つProjectileを活用します。

| キーバインド | SpaceMacs | Doom Emacs | 説明 |
|------------|-----------|------------|-------------|
| **SPC p p** | Switch projects | `projectile-switch-project` | アクティブプロジェクトの変更 |
| **SPC p f** | Find file in project | `projectile-find-file` | プロジェクトファイル検索 |
| **SPC p r** | Recent project files | `projectile-recentf` | プロジェクト内の最近のファイル |
| **SPC p d** | Find directory | `projectile-find-dir` | プロジェクトディレクトリ検索 |
| **SPC p t** | Show project tree | デフォルトなし | プロジェクトツリー表示 |
| **SPC p '** | Shell in project | デフォルトなし | プロジェクトルートでシェル |
| **SPC p k** | デフォルトなし | `projectile-kill-buffers` | プロジェクトバッファを削除 |
| **SPC p c** | デフォルトなし | `projectile-compile-project` | プロジェクトをコンパイル |
| **SPC p /** | デフォルトなし | Search in project | プロジェクト検索 |

両方のディストリビューションでProjectileが広く使用されていますが、Doomはデフォルトで追加のプロジェクト操作を含んでいます。treemacsが有効な場合、Doomではプロジェクトツリー切り替え（**SPC p t**）が競合し、代わりに**SPC o p**が必要になります。

## 8. その他の開発向けキーバインド

コアカテゴリを超えて、両方のディストリビューションは様々なユーティリティキーバインドを通じて広範な開発サポートを提供します。

### ヘルプシステム

| キーバインド | SpaceMacs | Doom Emacs | 説明 |
|------------|-----------|------------|-------------|
| **SPC h h** | Help menu | `help` | メインヘルプインターフェース |
| **SPC h k** | Describe key | `describe-key` | キーバインドの説明 |
| **SPC h f** | Describe function | `describe-function` | 関数ドキュメント |
| **SPC h v** | Describe variable | `describe-variable` | 変数ドキュメント |
| **SPC h b b** | Describe bindings | `describe-bindings` | 全キーバインドを表示 |
| **SPC h SPC** | SpaceMacs docs | 該当なし | ディストリビューションヘルプ |

### トグル機能

| キーバインド | SpaceMacs | Doom Emacs | 説明 |
|------------|-----------|------------|-------------|
| **SPC t n** | Toggle line numbers | デフォルトなし | 行番号表示 |
| **SPC t l** | Toggle truncate lines | `doom/toggle-line-numbers` | 行の折り返し/行番号 |
| **SPC t w** | Toggle whitespace | `whitespace-mode` | 空白文字の表示 |
| **SPC t s** | Toggle syntax checking | `flycheck-mode` | 構文チェッカー |
| **SPC t f** | Toggle fill column | `auto-fill-mode` | 自動補完モード（Doom） |

### エラーナビゲーション

| キーバインド | SpaceMacs | Doom Emacs | 説明 |
|------------|-----------|------------|-------------|
| **SPC e n** | Next error | **]e** | 次のエラーに移動 |
| **SPC e p** | Previous error | **[e** | 前のエラーに移動 |
| **SPC e l** | List errors | **SPC c x** | エラーリストを表示 |

### メジャーモードコマンド（カンマリーダー）

カンマキーは両方のディストリビューションでモード固有のコマンドへの高速アクセスポイントとして機能します：

| パターン | SpaceMacs | Doom Emacs | 典型的な機能 |
|---------|-----------|------------|-------------------|
| **, c** | Compile/execute | Compile/execute | コンパイルコマンド |
| **, d** | Debug operations | Debug operations | デバッグツール |
| **, e** | Evaluate/REPL | 標準化されていない | REPL相互作用 |
| **, f** | Format code | Format operations | コード整形 |
| **, g** | Go to/navigation | Generate/scaffold | ナビゲーションまたは生成 |
| **, r** | Refactoring | REPL/run commands | リファクタリングまたは実行 |
| **, t** | Test operations | Test operations | テスト実行 |

## 主要なアーキテクチャの違い

哲学的違いを理解することで、キーバインドのバリエーションが説明できます：

**SpaceMacsはニーモニックを重視** - すべてのプレフィックスは記憶に残るパターン（fはファイル、bはバッファ、gはgit）に従い、発見を直感的にします。システムは各プレフィックスレベルで利用可能なコマンドを示す広範なwhich-keyポップアップを提供します。

**Doomはパフォーマンスを優先** - キーバインドは遅延読み込みされ、システムはより効率的なデフォルト検索ツールを使用します。**SPC SPC**バインドはこれを例示し、文字ジャンプではなく高速プロジェクトファイルアクセスを提供します。

**モジュールシステムの違い** - SpaceMacsはレイヤーを使用し、Doomはモジュールを使用し、デフォルトで利用可能なキーバインドに影響します。Doomはターミナルサポートなどの機能で`init.el`での明示的なモジュール有効化が必要ですが、SpaceMacsはベースディストリビューションにより多くの機能を含みます。

**ローカルリーダーの実装** - 両方ともカンマを使用しますが、SpaceMacsは**SPC m**へのショートカットとして扱い、Doomは独立したローカルリーダーとして実装するため、ディストリビューション間の切り替え時にマッスルメモリに影響する可能性があります。

両方のディストリビューションで作業する開発者にとって、コアナビゲーション、ファイル、プロジェクト管理コマンドは大部分が互換性を保ち、検索インターフェース、gitステータスアクセス、ターミナル操作で主要な調整が必要になります。evil-modeとProjectileの共有基盤により、基本的な編集とプロジェクトワークフローは2つのシステム間でよく翻訳されます。