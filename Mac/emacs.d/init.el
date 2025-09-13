;;; ~/.emacs.d/init.el --- Emacsのメイン設定ファイル -*- lexical-binding: t; -*-

;;; Commentary:

;; === 対象者
;; Clojure開発者を想定
;; Vimキーバインドを利用
;; 日本語入力はddskkを利用

;; === 前提
;; macOS Sequoia 15.6
;; emacs-plus@30 で利用 (https://github.com/d12frosted/homebrew-emacs-plus)

;; === 依存関係
;; (1) gcc@15:  `brew install gcc@15`
;; (2) libgccjit@15 `brew install libgccjit@15`
;; (3) ripgrep `brew install ripgrep`
;; (4) 1Password CLI ([任意] SQLモードの項目でDB接続時のパスワード参照として使っています)
;; (5) JDK: `brew install --cask temurin21`
;; (6) Clojure CLI: `brew install clojure/tools/clojure`
;; (7) clojure-lsp: `brew install clojure-lsp/brew/clojure-lsp-native`
;; (8) docker/orbstack (コンテナを使うなら適宜インストール)
;; (9) Tree-sitterをコンパイルできるもの: `xcode-select --install`
;; (10) gls: `brew install coreutils` (diredでのファイル一覧表示に利用)

;; === 必要フォント
;; Source Han Code JP (https://github.com/adobe-fonts/source-han-code-jp)
;; UDEV Gothic 35NF (https://github.com/yuru7/udev-gothic)
;; JuliaMono (https://github.com/cormullion/juliamono/releases)

;; === SKK
;; SKKを使いこなすと日本語入力が楽しくなります
;; 以下をEmacs以前に動作するようにセットアップしておいてください(結構面倒です)
;; * macSKK
;;   * https://github.com/mtgto/macSKK
;;   * Emacs以外でもSKKを利用したい
;; * yaskkserv2
;;   * https://github.com/wachikun/yaskkserv2
;;   * SKK言語サーバー(自動でGoogle日本語入力連携)
;;   * Rustのcargoが必要
;; * macism
;;   * https://github.com/laishulu/macism
;;   * Emacs以外とのIME切り替えの摩擦を減らすために必要

;; === ~/.zshrc にあらかじめ以下を追記しておく
;; # eコマンドでサっとEmacsでファイルを開く
;; e() {
;;   if emacsclient --eval "t" > /dev/null 2>&1; then
;;     emacsclient -n "$@"
;;   else
;;     emacs "$@" &
;;   fi
;; }

;; === 初回のEmacs起動後に必要なコマンド
;; M-x nerd-icons-install-fonts (nerd-iconsのフォントをインストール)
;; M-x copilot-install-server (GitHub Copilotのサーバー)
;; M-x treesit-install-language-grammar (高速な構文解析)
;;   yaml, https://github.com/ikatyang/tree-sitter-yaml, 後はデフォルト
;;   json, https://github.com/tree-sitter/tree-sitter-json, 後はデフォルト

;;; Code:

;;====================================================================
;; ユーティリティ関数
;;===================================================================
;; === 時間計測
(defvar my-time-tmp (current-time))

(defun my-display-time ()
	"Displays the current time and the elapsed time since the last call."
	(let* ((now (current-time))
				 (diff (float-time (time-subtract now my-time-tmp))))
		(setq my-time-tmp now)
		(format "%s dt=%.6f"
						(format-time-string "%Y-%m-%d %H:%M:%S.%6N" now) diff)))

(defun my-insert-time ()
	"Insert measurement template at point."
	(interactive)
	(insert "(message \"[%s] %s\" (my-display-time) \"\")"))

;; === init.elを開く
(defun my-open-user-init ()
	"Open the user's init file."
	(interactive)
	(find-file user-init-file))

;; === カーソル下のシンボルが組み込みのパッケージかどうかチェック
(defun my-package-built-in-p (symbol)
	"Check if SYMBOL is a built-in package."
	(interactive (list (or (symbol-at-point)
												 (intern (read-string "Package: ")))))
	(message "[%s] built-in: %s" symbol (when (package-built-in-p symbol) t)))

;; === 現在のファイルのプロジェクトルートからのパスをコピー
(defun my-copy-project-relative-path ()
	"Copy the current file's path relative to the project root."
	(interactive)
	(if-let* ((proj (project-current))
						(root (project-root proj))
						(file (buffer-file-name)))
			(let ((relpath (file-relative-name file root)))
				(kill-new relpath)
				(message "Copied: %s" relpath))
		(user-error "Not visiting a file in a project.")))

;;===== TIPS / Rules of use-package ==================================
;; :ensure 組み込みパッケージにはnil, 外部パッケージにはtを指定
;; :vc GitHubやCodebergなどから直接インストールする場合に利用
;; :after 依存関係 (指定したパッケージのロード後に実行)
;; :init パッケージのロード前に実行
;; :custom defcustom変数はこちらで設定 (key value)の形式
;; :hook パッケージに関連するフックをコンスセル形式で設定 (hook . function)
;;   * 末尾が-hookならそのまま、そうでなければ-hookを付けたものが使われる。関数側は#'をつけない
;; :config 通常通りの処理をまとめる意味 (グローバルスコープになる)
;; :mode ファイル名からモード決定
;; :interpreter shebangからモード決定
;;====================================================================

;; === ローディング開始メッセージ
(message "[%s] %s" (my-display-time) "init.el loading...")

;;====================================================================
;; シェル環境変数をDockからの起動でも利用する
;;====================================================================

(use-package exec-path-from-shell
	:ensure t
	:config
	(exec-path-from-shell-initialize))

;;====================================================================
;; Emacs標準機能の設定
;;====================================================================

(use-package emacs
	:ensure nil
	:custom
	;; === インデントは空白で
	(indent-tabs-mode t)
	;; === インデントは2スペース
	(tab-width 2)
	(standard-indent 2)
	;; === MacのCommandをMetaキーに
	(mac-command-modifier 'meta)
	;; === ダイアログでのファイルオープンは使わない
	(use-file-dialog nil)
	;; === 他プロセスの編集をバッファに反映
	(global-auto-revert-mode t)
	;; === 長い行を含むファイルの最適化
	(global-so-long-mode t)
	;; === 削除したファイルをゴミ箱に移動
	(delete-by-moving-to-trash t)
	;; === シンボリックリンクを常に質問なしで開く
	(vc-follow-symlinks t)
	;; === 文字列を折り返さないのをデフォルトに
	(truncate-lines t)
	;; === ミニバッファでのyes/noの聞かれ方をy/nにする
	(use-short-answers t)
	;; === ファイル履歴の保存個数
	(recentf-max-saved-items 100)
	;; === ファイル履歴の保存
	(recentf-mode t)
	;; === コマンドの履歴を保存
	(savehist-mode t)
	;; === ウィンドウの状態を保持
	(winner-mode t)
	;; === 対応括弧を補完
	(electric-pair-mode t)
	;; === *scratch*バッファのデフォルトをtext-modeに
	(initial-major-mode 'text-mode)
	(initial-scratch-message "*scratch* for temporal notes\n\n")

	:config
	;; === C-hをBackspaceに、C-;をC-hに割り当て
	(define-key key-translation-map (kbd "C-h") (kbd "DEL"))
	(define-key key-translation-map (kbd "C-;") (kbd "C-h"))

	;; === 自分で配置したEmacsのソースコードへの参照を追加
	;; 利用しているEmacsバージョンによって適宜ソースコードはダウンロード必要
	(setq find-function-C-source-directory
				(concat "~/Documents/OSS/emacs/emacs-" emacs-version "/src"))

	;; === prog-modeとtext-modeのみ末尾の空白を表示
	(add-hook 'prog-mode-hook
						(lambda () (setq show-trailing-whitespace t)))
	(add-hook 'text-mode-hook
						(lambda () (setq show-trailing-whitespace t)))

	;; ===== Git管理外のファイルは、~/.cache/emacs/以下に保存する =====
	;; NOTE: elpa/, eln-cache/, tree-sitter/ はあえて除外している
	(defconst my-cache (expand-file-name "~/.cache/emacs/"))
	(dolist (d '("" "backups" "auto-saves" "auto-save-list" "undo-fu-session" "transient"
							 "copilot" "copilot-chat"))
		(make-directory (concat my-cache d "/") t))
	(setopt backup-directory-alist `(("." . ,(concat my-cache "backups/"))))
	(setopt auto-save-file-name-transforms `((".*" ,(concat my-cache "auto-saves/") t)))
	(setopt auto-save-list-file-prefix (concat my-cache "auto-save-list/.saves-"))
	(setopt recentf-save-file (concat my-cache "recentf"))
	(setopt savehist-file (concat my-cache "savehist"))
	(setopt bookmark-default-file (concat my-cache "bookmarks"))
	(setopt tramp-persistency-file-name (concat my-cache "tramp"))
	(setopt undo-fu-session-directory (concat my-cache "undo-fu-session/"))
	(setopt persp-state-default-file (concat my-cache "workspace-default"))
	(setopt transient-history-file (concat my-cache "transient/history.el"))
	(setopt transient-levels-file (concat my-cache "transient/levels.el"))
	(setopt transient-values-file (concat my-cache "transient/values.el"))
	(setopt project-list-file (concat my-cache "projects"))
	(setopt copilot-install-dir (concat my-cache "copilot/"))
	(setopt copilot-chat-default-save-dir (concat my-cache "copilot-chat/"))
	)

(use-package server
	:ensure nil
	:custom
	;; === eコマンドで開いたときに別ウィンドウで開く
	(server-window 'pop-to-buffer)
	:config
	;; === デーモン起動 (シェルの`e'コマンドから使う)
	(unless (server-running-p)
		(server-start)))

;; === which-keyのディレイ
(use-package which-key
	:ensure nil
	:custom
	(which-key-mode t)
	(which-key-idle-delay 0.3)
	(which-key-idle-secondary-delay 0)
	(which-key-sort-order nil))

;;====================================================================
;; bookmark
;;====================================================================

(use-package bookmark
	:ensure nil
	:custom
	(bookmark-save-flag 1) ; tではなく1で毎回保存
	:config
	(defun my-bookmark-set ()
		"Set a bookmark without prompting"
		(interactive)
		(let* ((line (line-number-at-pos))
					 (buf (or (file-name-nondirectory (or (buffer-file-name) ""))
										(buffer-name)))
					 (name (format "%s:L%d" (if (string-empty-p buf) (buffer-name) buf) line))
					 (record (bookmark-make-record)))
			(bookmark-store name record t))))

;;====================================================================
;; バッファの表示方法についての設定 (display-buffer-alist)
;;====================================================================

;; 良く使う分割方法
(defconst my-display-split
	'((display-buffer-pop-up-window
		 display-buffer-use-some-window)
		(side . right)
		(window-width . 0.5)))

;; 上に書いたものが優先される
(let ((rules
			 (list
				;; flymake-show-project-diagnostics
				`("\\*Flymake diagnostics" ,@my-display-split)
				;; vterm
				`("\\*vterm\\*" ,@my-display-split
					(window-parameters . ((dedicated . t))))
				;; ielm
				`("\\*ielm\\*" ,@my-display-split)
				;; dired上でdiredを開く時は同じウィンドウで開く(RETでその場で、S-RETで別のウィンドウで開く)
				`((lambda (buffer-name action)
						(and (with-current-buffer buffer-name (derived-mode-p 'dired-mode))
								 (with-current-buffer (window-buffer (selected-window))
									 (derived-mode-p 'dired-mode))))
					(display-buffer-same-window))
				;; diredを分割して開くようにする
				`((derived-mode . dired-mode) ,@my-display-split))))
	(setq display-buffer-alist (append rules display-buffer-alist)))

;;====================================================================
;; Emacs Lisp用の便利なHelp
;;====================================================================

;; === helpful
(use-package helpful
	:ensure t
	:bind
	(("C-h f" . helpful-callable)
	 ("C-h v" . helpful-variable)
	 ("C-h k" . helpful-key)
	 ("C-h x" . helpful-command)
	 ("C-h ." . helpful-at-point)))

;;====================================================================
;; 日本語入力
;;====================================================================

;; === ddskk
;; macism (https://github.com/laishulu/macism) のインストールが必要
(defun my-switch-ime (input-source)
	"Switch to INPUT-SOURCE when Emacs is focused (requires macism command)."
	(call-process "macism" nil 0 nil input-source))

(add-function :after after-focus-change-function
							(lambda ()
								(when (frame-focus-state)
									(my-switch-ime "net.mtgto.inputmethod.macSKK.ascii"))))

(use-package ddskk
	:ensure t
	:custom
	(skk-server-host "127.0.0.1")
	(skk-server-portnum 1178)
	(skk-dcomp-activate t)
	(skk-egg-like-newline t)
	(skk-delete-implies-kakutei nil)
	(skk-use-color-cursor nil)
	(skk-show-candidates-nth-henkan-char 3)
	(skk-isearch-mode-enable 'always)
	(skk-isearch-mode-string-alist '((hiragana . "")
																	 (katakana . "")
																	 (jisx0208-latin . "")
																	 (latin . "")
																	 (abbrev . "")
																	 (nil . "")))
	:hook
	(isearch-mode-hook . skk-isearch-mode-setup)
	(isearch-mode-hook . skk-latin-mode-on)
	(isearch-mode-end-hook . skk-isearch-mode-cleanup)
	(evil-normal-state-entry-hook . skk-latin-mode-on)
	(text-mode-hook . my-turn-on-skk)
	(prog-mode-hook . my-turn-on-skk)
	:bind
	("C-x j" . skk-mode)
	("C-j" . skk-kakutei)
	:config
	(defun my-turn-on-skk ()
		"skk-modeを有効にして、英字モードにする"
		(interactive)
		(skk-mode t)
		(skk-latin-mode-on)))

;;====================================================================
;; UIと外観 (フォントとテーマ)
;;====================================================================

;; === 現在行を強調表示
(global-hl-line-mode t)

;; === 行番号を表示
(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(add-hook 'text-mode-hook #'display-line-numbers-mode)

;; === カーソル位置の列番号をモードラインに表示
(column-number-mode t)

;; === tree-sitterによる色付けmax
(use-package treesit
	:ensure nil
	:custom
	(treesit-font-lock-level 4))

;; === フォント設定
(setq use-default-font-for-symbols nil)
;; Claude Codeの処理中の*マークのアニメーションでガタガタするのを防ぐ
(dolist (char '(#x00B7 #x2722 #x2733 #x2736 #x273B #x273D))
	(set-fontset-font t char (font-spec :family "JuliaMono")))
(set-fontset-font t 'han (font-spec :family "Source Han Code JP") nil 'prepend)
(set-face-attribute 'default nil :font "Source Han Code JP" :height 130)

;; === nerd iconsを利用
;; 初回にM-x nerd-icons-install-fontsの実行が必要(既にインストールされていれば不要)
(use-package nerd-icons
	:ensure t)

;; === カラーテーマ
(use-package catppuccin-theme
	:ensure t
	:custom
	(catppuccin-flavor 'macchiato)
	:config
	(load-theme 'catppuccin t))

;; === カーソルの色をオーバーライド
(set-cursor-color "#cad3f5")

;; === 対応カッコを色付け表示
(use-package rainbow-delimiters
	:ensure t
	:hook
	(prog-mode . rainbow-delimiters-mode))

;; === カラーコードを色付け
(use-package colorful-mode
	:ensure t
	:custom
	(colorful-use-prefix t)
	(global-colorful-mode t))

;; === doom-modeline
(use-package doom-modeline
	:ensure t
	:custom
	(doom-modeline-mode t)
	(doom-modeline-major-mode-icon nil) ; アイコン不要
	(doom-modeline-modal nil) ; evilのモード表示不要
	(doom-modeline-buffer-file-name-style 'file-name) ; ファイル名のみ
	(doom-modeline-percent-position nil) ; 位置の%表示不要
	(doom-modeline-buffer-encoding nil) ; LF/UTF-8などの表示不要
	)

;; === ダッシュボード
(use-package dashboard
	:ensure t
	:custom
	(dashboard-startup-banner 'logo)
	(dashboard-center-content t)
	(dashboard-vertically-center-content t)
	(dashboard-items '((recents . 10)
										 (bookmarks . 10)
										 (projects . 7)))
	:config
	(dashboard-setup-startup-hook))

;; === TODOハイライト
(use-package hl-todo
	:ensure t
	:custom
	(hl-todo-keyword-faces
	 `(("TODO" warning bold)
		 ("NOTE" ansi-color-cyan bold)
		 ("XXX" error bold)))
	(global-hl-todo-mode t))

;; === なめらかなホイールスクロール
(use-package ultra-scroll
	:ensure t
	:vc (:url "https://github.com/jdtsmith/ultra-scroll") ; if desired (emacs>=v30)
	:init
	(setq scroll-conservatively 3
				scroll-margin 0)
	:config
	;; :customでは適用されない
	(ultra-scroll-mode t))

;;====================================================================
;; EvilによるVimキーバインド
;;====================================================================

(use-package undo-fu
	:ensure t)
(use-package undo-fu-session
	:ensure t
	:after undo-fu
	:custom
	(undo-fu-session-global-mode t))

;; === evilによるVimキーバインドのエミュレート
(use-package evil
	:ensure t
	:after (undo-fu undo-fu-session)
	:custom
	(evil-want-integration t)
	(evil-want-keybinding nil)
	(evil-want-C-u-scroll t)
	(evil-undo-system 'undo-fu)
	(evil-symbol-word-search t) ; ひとかたまりで検索
	(evil-shift-width 2)
	(evil-mode t)
	:bind
	;; Emacsキーバインドも一部使う
	(:map evil-insert-state-map
				("C-f" . nil)
				("C-b" . nil)
				("C-n" . nil)
				("C-p" . nil)
				("C-a" . nil)
				("C-e" . nil)
				("C-d" . nil)
				("C-k" . nil)
				("C-y" . nil)
				("C-S-h" . #'backward-kill-sexp))
	(:map evil-motion-state-map
				("," . nil))
	:config
	;; 折り返しがある行でgj/gkが面倒なので
	(evil-global-set-key 'motion "j" 'evil-next-visual-line)
	(evil-global-set-key 'motion "k" 'evil-previous-visual-line)
	;; 最初からnormalモードであってほしいバッファ
	(evil-set-initial-state 'messages-buffer-mode 'normal)
	(evil-set-initial-state 'dashboard-mode 'normal))

;; === evilの便利なキーバインド追加
(use-package evil-collection
	:ensure t
	:after evil
	:custom
	(evil-collection-setup-minibuffer t)
	(evil-collection-setup-debugger-keys t)
	(evil-collection-key-blacklist '("C-j" "C-k" "SPC"))
	(evil-collection-magit-want-horizontal-movement t)
	:config
	(evil-collection-init))

;; === fdでESCできるように
(use-package evil-escape
	:ensure t
	:after evil
	:custom
	(evil-escape-mode t)
	:config
	;; isearch検索入力中にfキーの反応が遅れるのを防止
	(add-to-list 'evil-escape-inhibit-functions
							 (lambda () isearch-mode)))

;; === 囲み系の操作
(use-package evil-surround
	:ensure t
	:after evil
	:custom
	(global-evil-surround-mode t))

;; === 編集操作をハイライト
(use-package evil-goggles
	:ensure t
	:after evil
	:custom
	(evil-goggles-mode t)
	;; 削除系はハイライトせずとも分かる & 反映が遅れるとストレスなのでオフ
	(evil-goggles-enable-delete nil)
	(evil-goggles-enable-change nil))

;; === 検索ヒット件数を表示
(use-package evil-anzu
	:ensure t
	:after evil
	:custom
	(global-anzu-mode t))

;; === コメントアウト
(use-package evil-commentary
	:ensure t
	:config
	(evil-commentary-mode t))

;; === 数値のインクリメント
(use-package evil-numbers
	:ensure t
	:after evil
	:bind
	(:map evil-normal-state-map
				("C-a" . evil-numbers/inc-at-pt)))

;;====================================================================
;; ファイルツリー (dired-subtree)
;;====================================================================

;; === dired上でTABでサブディレクトリを展開できる
(use-package dired-subtree
	:ensure t
	:custom
	;; diredのオプションだがここに書く
	(dired-dwim-target t)
	(insert-directory-program "gls")
	(dired-listing-switches "-alhG --time-style=long-iso"))

;;====================================================================
;; ミニバッファ内での検索・候補選択
;;====================================================================

;; === 便利な統合コマンドの提供 (consult)
(use-package consult
	:ensure t
	:custom
	(consult-async-min-input 2)
	(consult-ripgrep-args "rg --null --line-buffered --color=never --max-columns=1000 --path-separator /   --smart-case --no-heading --with-filename --line-number --search-zip --hidden --glob=!.git --glob=!node_modules/* --glob=!target/* --glob=!*.lock")
	:hook
	(completion-list-mode . consult-preview-at-point-mode))

;; === 補完候補を垂直に表示するUI (vertico)
(use-package vertico
	:ensure t
	:custom
	(vertico-mode t)
	(vertico-cycle t)
	(vertico-count 15)
	(vertico-resize nil))

;; === 柔軟な絞り込みスタイル (orderless)
(use-package orderless
	:ensure t
	:custom
	(completion-styles '(basic partial-completion orderless))
	(completion-category-defaults nil)
	(completion-category-overrides '((file (styles basic))
																	 (corfu (styles basic partial-completion orderless))))
	;; 正規表現とあいまい検索もデフォルトで有効に
	;; 完全一致(literal)を優先させたいときは先頭に`='をつける
	(orderless-matching-styles '(orderless-literal
															 orderless-flex
															 orderless-regexp)))

;; === 補完候補に注釈を追加 (marginalia)
(use-package marginalia
	:ensure t
	:after vertico
	:custom
	(marginalia-mode t))

;; === 候補に対するアクション (embark)
(use-package embark
	:ensure t
	:bind
	(("C-." . embark-act)
	 ("C-," . embark-export)))

;; === embarkをconsultから使う (embark-consult)
(use-package embark-consult
	:ensure t
	:after (embark consult)
	:hook
	(embark-collect-mode . consult-preview-at-point-mode))

;; === embark-exportしたバッファを直接編集して一括置換などを実現する (wgrep)
(use-package wgrep
	:ensure t
	:custom
	(wgrep-auto-save-buffer t)
	(wgrep-change-readonly-file t))

;;===== Workflow of Replace =========================================
;; 1. `SPC s s' (consult-line)や `SPC s p' (consult-ripgrep)で候補表示
;; 2. `C-,' (embark-export)でembark-collect-modeに
;; 3. OccurやWgrepの違いはあるが, `i'で編集モードに入る
;; 4. `:%s;xxx;yyy;g' などで一括置換 (普通に編集してもいい)
;; 5. `ESC'で編集モードを抜ける (この際に変更を保存するか聞かれることもある)
;;====================================================================

;;====================================================================
;; バッファ内のインライン/ポップアップ補完
;;====================================================================

;; === バッファ内補完のUIフロントエンド (corfu)
(use-package corfu
	:ensure t
	:custom
	(global-corfu-mode t)
	(corfu-popupinfo-mode t)
	(corfu-history-mode t)
	(corfu-auto t)
	(corfu-auto-delay 0)
	(corfu-popupinfo-delay 0)
	(corfu-auto-prefix 1)
	(corfu-cycle t)
	(corfu-preselect 'prompt)
	(corfu-quit-no-match 'separator)
	(tab-always-indent 'complete)
	(corfu-separator ?\s))

;; === 補完ポップアップ内のアイコン
(use-package nerd-icons-corfu
	:ensure t
	:after (corfu nerd-icons)
	:config
	(add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

;;====================================================================
;; LSP (eglot)
;;====================================================================

;; === lsp (eglot)
(use-package eglot
	:ensure nil
	:hook
	(clojure-ts-mode . eglot-ensure)
	(zig-mode . eglot-ensure)
	(terraform-mode . eglot-ensure)
	:custom
	(eglot-events-buffer-config '(:size nil :format full))
	(eglot-autoshutdown t)
	(eglot-connect-timeout 120)
	(eglot-extend-to-xref nil)
	(eldoc-echo-area-use-multiline-p nil)
	:config
	;; === eglotによるLSP起動
	(defun my-eglot-start ()
		"Start eglot for the current buffer if not already started."
		(interactive)
		(eglot-ensure)))

;; === スニペット・テンプレート (tempel)
(use-package tempel
	:ensure t)

(use-package eglot-tempel
	:ensure t
	:init (eglot-tempel-mode t))

;; === 補完ソースの統合・拡張 (cape)
(use-package cape
	:ensure t
	:hook
	(prog-mode . my-prog-capf)
	(text-mode . my-text-capf)
	:config
	(defun my-prog-capf ()
		(unless (local-variable-p 'my-prog-capf-configured)
			(if (bound-and-true-p eglot--managed-mode)
					(setq-local completion-at-point-functions
											(cons (cape-capf-super #'eglot-completion-at-point
																						 #'tempel-expand
																						 #'cape-file)
														completion-at-point-functions))
				(setq-local completion-at-point-functions
										(cons (cape-capf-super #'tempel-complete
																					 #'cape-file)
													completion-at-point-functions))))
		(setq-local my-prog-capf-configured t))

	(defun my-text-capf ()
		(unless (local-variable-p 'my-text-capf-configured)
			(setq-local completion-at-point-functions
									(cons (cape-capf-super #'tempel-complete
																				 #'cape-dabbrev
																				 #'cape-file)
												completion-at-point-functions)))
		(setq-local my-text-capf-configured t))
	)

;;====================================================================
;; ターミナル (vterm)
;;====================================================================
(use-package vterm
	:ensure t
	:after evil
	:custom
	(vterm-tramp-shells '(("ssh" "/bin/bash")
												("scp" "/bin/bash")
												("docker" "/bin/bash")))
	:hook
	(vterm-mode . (lambda ()
									;; Claude Code IDEなどでnbspが青く可視化されるのが気に食わないため
									(setq-local nobreak-char-display 'nil)
									;; insertモードで起動
									(evil-insert-state)))
	:bind
	("C-'" . my-toggle-vterm)
	:config
	;; vterm-toggleパッケージは使わない(claude-code-ideとの兼ね合い)
	(defun my-toggle-vterm ()
		"Toggle vterm terminal."
		(interactive)
		(let ((vterm-window (get-buffer-window "*vterm*")))
			(if (and vterm-window (eq (selected-window) vterm-window))
					(quit-window)
				(vterm)))))

;;====================================================================
;; Git操作 (magit・diff-hl・vc)
;;====================================================================

;; === magit
(use-package magit
	:ensure t
	:custom
	(magit-diff-refine-hunk 'all)
	:config
	;; NOTE 差分表示の色合いをカタムするために複雑なことをしているが、しなくてもいい
	;; magit-diff-visit-fileは別ウィンドウで開く
	(advice-add 'magit-diff-visit-file :around
							(lambda (orig-fun &rest _args)
								(funcall orig-fun t)))

	;; magit-diffのとき、vc-diffを使う。未追跡ファイルは単に開く
	(defun my-magit-diff-dwim-with-vc-diff (orig-fun &rest args)
		"Advice function to use `vc-diff` in `magit-status-mode`."
		(if-let (file (and (derived-mode-p 'magit-status-mode)
											 (magit-file-at-point)))
				(if (null (vc-state file))
						(progn
							(message "%s is untracked file." file)
							(view-file-other-window file))
					(with-current-buffer (find-file-noselect file)
						(call-interactively #'vc-diff)))
			(apply orig-fun args)))

	(advice-add 'magit-diff-dwim :around #'my-magit-diff-dwim-with-vc-diff))

;; === フリンジに差分を強調表示 (diff-hl)
(use-package diff-hl
	:ensure t
	:custom
	(global-diff-hl-mode t)
	(diff-hl-flydiff-mode t)
	:hook
	(magit-pre-refresh-hook  . diff-hl-magit-pre-refresh)
	(magit-post-refresh-hook . diff-hl-magit-post-refresh))

;;====================================================================
;; ワークスペース (perspective.el)
;;====================================================================

(use-package perspective
	:ensure t
	:init
	(setq persp-suppress-no-prefix-key-warning t)
	(persp-mode)
	:custom
	(persp-sort 'created)
	(persp-modestring-short t))

;;====================================================================
;; Shell Script
;;====================================================================

(use-package sh-script
	:ensure nil
	:mode (("\\.\\(sh\\|bash\\)\\'" . sh-mode) ; sh/bash
				 ("\\.\\(bashrc\\|bash_profile\\)\\'" . sh-mode) ; bash
				 ("\\.?zsh\\(rc\\|env\\|profile\\)?\\'" . sh-mode)) ; zsh
	:interpreter (("sh"   . sh-mode)
								("bash" . sh-mode)
								("zsh"  . sh-mode))
	:custom
	(sh-basic-offset 2)
	(sh-indentation  2))

;;====================================================================
;; Clojure/ClojureScript/ClojureDart
;;
;; [依存関係]
;; (1) JDK: `brew install --cask temurin21`
;; (2) Clojure CLI: `brew install clojure/tools/clojure`
;; (3) clojure-lsp: `brew install clojure-lsp/brew/clojure-lsp-native`
;; (4) docker/orbstack
;; (5) Tree-sitterをコンパイルできるもの: `xcode-select --install`
;;====================================================================

;; === clojure-ts-mode
;; 従来のclojure-modeではなくclojure-ts-modeで置き換える
;; (.clj,.cljc,.cljs,.cljd,.edn自動認識)
(use-package clojure-ts-mode
	:ensure t
	:custom
	(clojure-ts-toplevel-inside-comment-form t)
	:config
	;; === clojure-lsp用キャッシュの削除 & 再起動
	(defun my-clojure-lsp-clear-cache-and-restart ()
		"Clear clojure-lsp cache and restart the server."
		(interactive)
		(call-interactively #'eglot-shutdown)
		(let* ((root-dir (project-root (project-current)))
					 (lsp-cache (file-name-concat root-dir ".lsp/.cache"))
					 (kondo-cache (file-name-concat root-dir ".clj-kondo/.cache")))
			(when (file-directory-p lsp-cache)
				(delete-directory lsp-cache t)
				(message "[%s] Deleted clojure-lsp cache at %s" (my-display-time) lsp-cache))
			(when (file-directory-p kondo-cache)
				(delete-directory kondo-cache t)
				(message "[%s] Deleted clj-kondo cache at %s" (my-display-time) kondo-cache))
			)
		(eglot-ensure)))

(use-package cider
	:ensure t
	:hook (clojure-ts-mode . cider-mode)
	:custom
	(cider-repl-buffer-size-limit 10000)
	(cider-font-lock-dynamically '(macro core function var deprecated))
	(cider-repl-pop-to-buffer-on-connect nil)
	(setq cider-cljs-comple)
	(cider-use-xref nil))

;; 構造的編集 (puni)
(use-package puni
	:ensure t
	:demand t
	:hook
	((emacs-lisp-mode . puni-mode)
	 (lisp-interaction-mode . puni-mode)
	 (clojure-ts-mode . puni-mode))
	:config
	;; === puniでカーソル以降をそのレベルで閉じるまで削除
	(defun my-puni-kill-to-end ()
		"Kill to the end of the current sexp."
		(interactive)
		(let ((end (save-excursion (puni-end-of-sexp) (point))))
			(kill-region (point) end)))

	;; === puniでシンボルを""で囲む
	(defun my-wrap-symbol-with-quotes ()
		"Surround the current symbol with double quotes."
		(interactive)
		(let ((bounds (bounds-of-thing-at-point 'symbol)))
			(when bounds
				(save-excursion
					(goto-char (cdr bounds))
					(insert "\"")
					(goto-char (car bounds))
					(insert "\""))))))

;; Javaライブラリのジャンプ時などに
(use-package jarchive
	:ensure t
	:after eglot
	:config
	(jarchive-setup))

;;====================================================================
;; Markdown
;;====================================================================

(use-package markdown-mode
	:ensure t
	:mode ("\\.md\\'" . gfm-mode)
	:init
	(setq markdown-command '("pandoc" "--from=markdown" "--to=html5"))

	:custom
	(markdown-fontify-code-blocks-natively t)
	(markdown-indent-on-enter 'indent-and-new-item)
	(markdown-gfm-use-electric-backquote nil))

;;====================================================================
;; GitHub Copilot連携
;;====================================================================

;; 初回起動後にM-x copilot-install-serverが必要
(use-package copilot
	:ensure t
	:vc (:url "https://github.com/copilot-emacs/copilot.el" :rev :newest :branch "main")
	:hook
	;; (prog-mode . copilot-mode)
	(copilot-chat-org-prompt-mode . copilot-mode) ; チャット内自体でも有効化
	:bind
	(:map copilot-completion-map
				("C-<tab>" . copilot-accept-completion))
	(:map prog-mode-map
				("M-/" . copilot-complete))
	:custom (copilot-max-char 1000000)    ; 最大文字数を増やす
	:config
	(add-to-list 'copilot-indentation-alist '(prog-mode  2))
	(add-to-list 'copilot-indentation-alist '(org-mode  2))
	(add-to-list 'copilot-indentation-alist '(text-mode  2))
	(add-to-list 'copilot-indentation-alist '(emacs-lisp-mode  2)))

(use-package copilot-chat
	:ensure t)

;;====================================================================
;; Claude Code IDE
;;====================================================================
(use-package claude-code-ide
	:ensure t
	:vc (:url "https://github.com/manzaltu/claude-code-ide.el" :rev :newest)
	:custom
	(claude-code-ide-window-width 0.4)
	(claude-code-ide-focus-claude-after-ediff nil)
	:config
	(claude-code-ide-emacs-tools-setup)

	;; 崩れないように*claude-code[...]*バッファのフォントを変更
	(defun my-set-font-for-claude-buffer ()
		"Set a specific font for Claude Code IDE buffers."
		(when (string-match-p "^\\*claude-code" (buffer-name))
			(buffer-face-set :family "UDEV Gothic 35NF" :height 130)))
	(add-hook 'buffer-list-update-hook #'my-set-font-for-claude-buffer)

	;; スクラッチバッファのトグル表示
	(defun my-claude-code-ide-scratch ()
		"Toggle Claude Code scratch buffer."
		(interactive)
		(let* ((project-dir (claude-code-ide--get-working-directory))
					 (buffer-name (format "*claude-scratch[%s]*"
																(file-name-nondirectory (directory-file-name project-dir))))
					 (scratch-buffer (get-buffer buffer-name))
					 (scratch-window (and scratch-buffer (get-buffer-window scratch-buffer))))

			(cond
			 ;; ウィンドウが表示されている場合は閉じる
			 (scratch-window
				(delete-window scratch-window))

			 ;; バッファは存在するが非表示の場合は表示する
			 (scratch-buffer
				(my-claude-code-ide-scratch-show scratch-buffer project-dir))

			 ;; バッファが存在しない場合は作成して表示
			 (t
				(let ((claude-buffer (get-buffer (claude-code-ide--get-buffer-name project-dir))))
					(unless claude-buffer
						(user-error "Claude Code IDEが起動していません"))
					(let ((new-buffer (get-buffer-create buffer-name)))
						(with-current-buffer new-buffer
							(insert "*Claude Code IDE scratch*\n\n")
							(setq-local claude-scratch-project-dir project-dir)
							(setq-local truncate-lines nil))  ; 折り返しを有効化
						(my-claude-code-ide-scratch-show new-buffer project-dir)))))))

	;; スクラッチバッファを表示する
	(defun my-claude-code-ide-scratch-show (buffer _project-dir)
		"Show scratch BUFFER below the leftmost window of the current frame."
		(let* ((base (frame-first-window))  ;; フレームの一番左上の live window
					 (win  nil))
			(when (window-live-p base)
				(condition-case _
						(progn
							;; 左端ウィンドウの直下に 12 行で割って表示
							(setq win (split-window base -12 'below))
							(set-window-buffer win buffer))
					(error
					 ;; 何かで split に失敗しても、とにかく表示はする
					 (setq win (display-buffer buffer '((display-buffer-pop-up-window)))))))
			(when (window-live-p win)
				(set-window-dedicated-p win t)
				(select-window win)
				(goto-char (point-max)))
			win))

	;; Claude Code IDE とスクラッチバッファの起動・トグル
	(defun my-claude-code-ide-with-scratch ()
		"Toggle Claude Code IDE with scratch buffer."
		(interactive)
		(let* ((project-dir (claude-code-ide--get-working-directory))
					 (claude-buffer-name (claude-code-ide--get-buffer-name project-dir))
					 (claude-buffer (get-buffer claude-buffer-name))
					 (claude-window (and claude-buffer (get-buffer-window claude-buffer)))
					 (scratch-buffer-name (format "*claude-scratch[%s]*"
																				(file-name-nondirectory (directory-file-name project-dir))))
					 (scratch-buffer (get-buffer scratch-buffer-name))
					 (scratch-window (and scratch-buffer (get-buffer-window scratch-buffer))))

			(cond
			 ;; 両方が表示されている場合は両方を隠す
			 ((and claude-window scratch-window)
				(claude-code-ide)  ; Claude Codeのトグル
				(delete-window scratch-window))

			 ;; Claude Codeのみ表示されている場合は隠す
			 (claude-window
				(claude-code-ide))  ; Claude Codeのトグル

			 ;; スクラッチバッファのみ表示されている場合は両方表示
			 (scratch-window
				(claude-code-ide)  ; Claude Codeを表示
				(my-claude-code-ide-scratch-show scratch-buffer project-dir)
				;; スクラッチバッファにフォーカスを移す
				(select-window (get-buffer-window scratch-buffer)))

			 ;; Claude バッファは存在するがウィンドウがない場合
			 (claude-buffer
				(claude-code-ide)  ; Claude Codeを表示
				(when (or scratch-buffer (not claude-buffer))
					(my-claude-code-ide-scratch))
				(let ((scratch-win (get-buffer-window (get-buffer scratch-buffer-name))))
					(when scratch-win
						(select-window scratch-win))))

			 ;; 他のケースは両方表示
			 (t
				(claude-code-ide)  ; Claude Codeを表示
				;; スクラッチバッファも表示
				(when (or scratch-buffer (not claude-buffer))
					(my-claude-code-ide-scratch))
				;; スクラッチバッファにフォーカスを移す
				(let ((scratch-win (get-buffer-window (get-buffer scratch-buffer-name))))
					(when scratch-win
						(select-window scratch-win)))))))

	;; 選択範囲の送信またはプロンプト入力
	(defun my-claude-code-ide-send-region-or-prompt ()
		"Send Evil selection to Claude Code or open prompt."
		(interactive)
		(if (and (bound-and-true-p evil-mode)
						 (or (evil-visual-state-p)
								 (region-active-p)))
				;; Evil visual mode での選択がある場合
				(let ((text (buffer-substring-no-properties (region-beginning) (region-end))))
					(when (evil-visual-state-p)
						(evil-exit-visual-state))
					(my-claude-code-ide-send-text text))
			;; 選択していない場合はデフォルトの prompt コマンド
			(claude-code-ide-send-prompt)))

	;; 数字を送信するコマンド
	(defun my-claude-code-ide-send-number-1 ()
		"Send '1' to Claude Code."
		(interactive)
		(my-claude-code-ide-send-text "1"))

	(defun my-claude-code-ide-send-number-2 ()
		"Send '2' to Claude Code."
		(interactive)
		(my-claude-code-ide-send-text "2"))

	(defun my-claude-code-ide-send-number-3 ()
		"Send '3' to Claude Code."
		(interactive)
		(my-claude-code-ide-send-text "3"))

	;; テキストを Claude Code に送信
	(defun my-claude-code-ide-send-text (text)
		"Send TEXT to Claude Code."
		(let* ((project-dir (claude-code-ide--get-working-directory))
					 (claude-buffer-name (claude-code-ide--get-buffer-name project-dir))
					 (claude-buffer (get-buffer claude-buffer-name)))
			(unless claude-buffer
				(user-error "Claude Code IDEが起動していません"))
			(with-current-buffer claude-buffer
				(claude-code-ide--terminal-send-string text)
				(sit-for 0.1)
				(claude-code-ide--terminal-send-return))
			(message "Claude Codeに送信しました: %s" (substring text 0 (min 50 (length text))))))
	)

;;====================================================================
;; Dockerコンテナ内開発ワークフロー
;;====================================================================

(use-package dockerfile-mode
	:ensure t
	:mode ("Dockerfile\\'" . dockerfile-mode))

;; === yaml-ts-mode
;; 初回起動時に`M-x treesit-install-language-grammar`を実行し
;; languageとして`yaml`, 手動でURL: https://github.com/ikatyang/tree-sitter-yamlを指定(後はデフォルト)
(use-package yaml-ts-mode
	:ensure nil
	:mode ("\\.ya?ml\\'" . yaml-ts-mode))

;; === json-ts-mode
;; 初回起動時に`M-x treesit-install-language-grammar`を実行し
;; languageとして`json`, 手動でURL: https://github.com/tree-sitter/tree-sitter-jsonを指定(後はデフォルト)
(use-package json-ts-mode
	:ensure nil
	:mode ("\\.json\\'" . json-ts-mode))

(use-package docker
	:ensure t
	:custom
	(docker-container-columns
	 '((:name "Names" :width 30 :template "{{ json .Names }}" :sort nil :format nil)
		 (:name "Status" :width 30 :template "{{ json .Status }}" :sort nil :format nil)
		 (:name "Ports" :width 30 :template "{{ json .Ports }}" :sort nil :format nil)))
	(docker-container-default-sort-key '("Names")))

;; docker compose upなどでコンテナが起動していれば
;; M-x find-fileから/docker:コンテナ名:/path/to/fileで接続すればlspもうまく動作
;; コンテナのシェルにアタッチしたときに、C-p, C-nが2回必要な問題は、config.jsonに以下を記載
;; { "detachKeys": "ctrl-z,z" }

;;====================================================================
;; DB接続
;;====================================================================

(use-package sql
	:ensure nil
	:custom
	(sql-postgres-login-params nil)
	(setq sql-connection-alist
				'((sample-postgres
					 (sql-product 'postgres)
					 ;; NOTE: DB設定については適宜変更
					 (sql-database (concat
													"postgresql://"
													"root"
													":" (my-read-1password "sample_dev_db")
													"@localhost"
													":5432"
													"/sample_dev"
													)))))
	:config
	(setq sql-mode-hook
				`(lambda ()
					 (sql-indent-enable)
					 (sql-highlight-postgres-keywords)))

	(defun my-read-1password (name)
		"1Passwordからパスワードを取得する"
		(let ((password (shell-command-to-string
										 (format "op read op://Private/%s/password" name))))
			(string-trim password))))

;;====================================================================
;; Zig
;;====================================================================

;; brew install zig
;; brew install zls
(use-package zig-mode
	:ensure t
	:mode "\\.\\(zig\\|zon\\))\\'")

;;====================================================================
;; Web開発系
;;====================================================================
(use-package js
	:ensure nil
	:custom
	(js-indent-level 2))

;;====================================================================
;; Groovy DSL (.gradle)
;;====================================================================
(use-package groovy-mode
	:ensure t
	:mode "\\.gradle\\'")

;;====================================================================
;; Terraform (.tf)
;;====================================================================
(use-package terraform-mode
	:ensure t
	:mode "\\.tf\\'")

;;====================================================================
;; Format On Save設定の集約
;;====================================================================

;; === Emacs Lisp
(defun my-format-emacs-lisp ()
	"Format the current buffer as Emacs Lisp code."
	(interactive)
	(save-excursion
		(indent-region (point-min) (point-max)))
	(message "[elisp] formatted."))

;; === Clojure
(defun my-format-clojure ()
	"Format the current buffer as Clojure code using cljfmt."
	(interactive)
	;; プロジェクトルートでのcljfmtの実行
	;; バッファを消して再度挿入なのでsave-excursionは使えない
	(when-let* ((cljfmt-path (executable-find "cljfmt"))
							(project-root-path (project-root (project-current))))
		(let ((p-current (point))
					(default-directory project-root-path))
			(call-process-region (point-min) (point-max) cljfmt-path t t nil "fix" "-" "--quiet")
			(goto-char p-current)
			(message "[cljfmt] Formatted."))))

;; === フォーマッタの適用方法をここにまとめる
;; 左: メジャーモード, 右: 優先順位をつけたフォーマット方法のリスト
(defvar my-format-rules
	'((emacs-lisp-mode . (my-format-emacs-lisp))
		(clojure-ts-mode . (:lsp my-format-clojure))
		(clojure-ts-clojurescript-mode . (:lsp my-format-clojure))
		(clojure-ts-clojurec-mode . (:lsp my-format-clojure))
		(clojure-ts-clojuredart-mode . (:lsp my-format-clojure))))

(defun my-format-try (formatter)
	"Try to format using FORMATTER."
	(pcase formatter
		(:lsp
		 (when (bound-and-true-p eglot--managed-mode)
			 (message "[eglot] Formatting via LSP...")
			 (call-interactively #'eglot-format-buffer)
			 t))

		((and (pred fboundp) fn)
		 (funcall fn)
		 t)

		(_ nil)))

(defun my-format-buffer ()
	"Format the current buffer based on its major mode."
	(interactive)
	(let ((formatters (cdr (assoc major-mode my-format-rules))))
		(if (not (cl-some #'my-format-try formatters))
				(message "No suitable formatter found for %s" major-mode))))

(add-hook 'before-save-hook #'my-format-buffer)
(add-hook 'before-save-hook #'whitespace-cleanup) ;; trailing spacesの削除

;;====================================================================
;; キーバインド (general.el)
;;====================================================================

;; use-packageと:generalの組み合わせで色々できる
(use-package general
	:ensure t
	:after (evil evil-collection)
	:config
	(general-evil-setup)
	(general-auto-unbind-keys)

	;; === リーダーキー定義 (SPC)
	(general-create-definer my-global-leader-def
		:states '(normal visual)
		:keymaps 'override
		:prefix "SPC")

	;; === ローカルリーダーキー定義 (,)
	(general-create-definer my-local-leader-def
		:states '(normal)
		:keymaps 'override
		:prefix ",")

	;; === モーション系 (g)
	(general-create-definer my-motion-leader-def
		:states '(normal visual)
		:keymaps 'override
		:prefix "g")

	;; === 全メジャーモード共通のキーバインド
	(my-global-leader-def
		;; (SPC) M-x
		"SPC" '(execute-extended-command :wk "M-x")

		;; (;) コメント
		";" '(evil-commentary-line :wk "comment")

		;; (t) トグル
		"t" '(:ignore t :wk "Toggle")
		"t l" '(toggle-truncate-lines :wk "truncate line")
		"t f" '(flymake-mode :wk "toggle flymake")
		"t c" '(copilot-mode :wk "toggle copilot")

		;; (q) 終了操作
		"q" '(:ignore t :wk "Quit")
		"q q" '(save-buffers-kill-terminal :wk "quit")
		"q r" '(restart-emacs :wk "restart")

		;; (f) ファイル操作
		"f" '(:ignore t :wk "Files")
		"f f" '(find-file :wk "file find")
		"f r" '(recentf-open :wk "file recent")
		"f p" '(project-find-file :wk "find in project")
		"f s" '(save-buffer :wk "file save")
		"f i" '(my-open-user-init :wk "init.el")
		"f t" '(project-dired :wk "dired")
		"f y" '(my-copy-project-relative-path :wk "copy relative path")

		;; (b) バッファ操作/ブックマーク
		"b" '(:ignore t :wk "Buffers/Bookmark")
		"b b" '(consult-buffer :wk "buffer switch")
		"b d" '(kill-current-buffer :wk "buffer kill")
		"b h" '(dashboard-open :wk "dashboard home")
		"b l" '(consult-bookmark :wk "bookmark list")
		"b s" '(my-bookmark-set :wk "bookmark set")
		"b k" '(bookmark-delete :wk "bookmark delete")

		;; (s) 検索
		"s" '(:ignore t :wk "Search")
		"s s" '(consult-line :wk "search in buffer")
		"s p" '(consult-ripgrep :wk "search in project")
		"s f" '(consult-flymake :wk "search flymake")

		;; (p) プロジェクト管理
		"p" '(:ignore t :wk "Project/Package")
		"p p" '(project-switch-project :wk "project switch")
		"p f" '(flymake-show-project-diagnostics :wk "project flymake")

		;; (w) ワークスペース/ウィンドウ操作
		"w" '(:ignore t :wk "Workspace/Window")
		"w w" '(persp-switch :wk "workspace switch")
		"w r" '(persp-rename :wk "workspace rename")
		"w d" '(persp-kill :wk "workspace kill")
		"w s" '(persp-state-save :wk "workspace save")
		"w l" '(persp-state-load :wk "workspace load")
		"w u" '(winner-undo :wk "window undo")

		;; (g) Git/ジャンプ
		"g" '(:ignore t :wk "Git/GoTo")
		"g s" '(magit-status-quick :wk "git status")
		"g l" '(magit-log-current :wk "git log")
		"g d" '(vc-diff :wk "git diff")

		;; (d) 差分/デバッグ/Docker/DB
		"d" '(:ignore t :wk "Diff/Debug/Docker/DB")
		"d d" '(diff-hl-show-hunk :wk "diff")
		"d c" '(docker-containers :wk "docker containers")
		"d b" '(sql-connect :wk "db connect")

		;; (a) 生成AI系
		"a" '(:ignore t :wk "AI")
		"a p" '(copilot-chat :wk "Copilot chat") ; 補完はM-/でサジェスト
		;; Claude Code IDE (M-RET: 改行, C-ESC: エスケープ)
		"a m" '(claude-code-ide-menu :wk "Claude menu")
		"a a" '(my-claude-code-ide-with-scratch  :wk "Claude start")
		"a b" '(my-claude-code-ide-scratch :wk "Claude scratch buffer")
		"a i" '(claude-code-ide-insert-at-mentioned :wk "Claude insert at mentioned")
		"a s" '(my-claude-code-ide-send-region-or-prompt :wk "Claude send prompt")
		"a n" '(claude-code-ide-insert-newline :wk "Claude insert newline")
		"a 1" '(my-claude-code-ide-send-number-1 :wk "Claude send '1'")
		"a 2" '(my-claude-code-ide-send-number-2 :wk "Claude send '2'")
		"a 3" '(my-claude-code-ide-send-number-3 :wk "Claude send '3'")
		"a e" '(claude-code-ide-send-escape :wk "Claude send escape")
		"a q" '(claude-code-ide-stop :wk "Claude stop")
		"a c" '(claude-code-ide-continue :wk "Claude continue")
		"a r" '(claude-code-ide-resume :wk "Claude resume")
		"a l" '(claude-code-ide-list-sessions :wk "Claude list sessions")

		;; (l) LSP (eglot) 操作
		"l" '(:ignore t :wk "LSP")
		"l s" '(my-eglot-start :wk "lsp start")
		)

	;; LSP (eglot) 操作 (SPC l)
	(my-global-leader-def
		:keymaps '(eglot-mode-map)
		"l r" '(eglot-rename :wk "rename symbol")
		"l a" '(eglot-code-actions :wk "code actions")
		"l f" '(eglot-format :wk "format")
		"l R" '(eglot-reconnect :wk "lsp reconnect")
		"l q" '(eglot-shutdown :wk "lsp shutdown")
		)

	;; === ジャンプなど (g系)
	(my-motion-leader-def
		"n" '(diff-hl-next-hunk :wk "next change")
		"p" '(diff-hl-previous-hunk :wk "prev change")
		"t" '(persp-next :wk "next workspace")
		"T" '(persp-prev :wk "prev workspace")
		)

	(my-motion-leader-def
		:keymaps '(dired-mode-map)
		"r" '(dired-subtree-revert :wk "revert dired subtree")
		)

	;; === Lisp系の編集操作 (,)
	(my-local-leader-def
		:keymaps '(emacs-lisp-mode-map
							 lisp-interaction-mode-map
							 clojure-ts-mode-map
							 clojure-ts-clojurescript-mode-map
							 clojure-ts-clojurec-mode-map
							 clojure-ts-clojuredart-mode-map)
		"s" '(puni-slurp-forward :wk "slurp forward")
		"S" '(puni-slurp-backward :wk "slurp backward")
		"b" '(puni-barf-forward :wk "barf forward")
		"B" '(puni-barf-backward :wk "barf backward")
		"r" '(puni-raise :wk "raise sexp")
		"w" '(:ignore t :wk "wrap")
		"w 9" '(puni-wrap-round :wk "wrap ()")
		"w [" '(puni-wrap-square :wk "wrap []")
		"w {" '(puni-wrap-curly :wk "wrap {}")
		"w \"" '(my-wrap-symbol-with-quotes :wk "wrap \"\"")
		"d" '(:ignore t :wk "delete")
		"d w" '(puni-splice :wk "delete wrap")
		"k" '(my-puni-kill-to-end :wk "kill to sexp end")
		"h" '(eldoc :wk "eldoc")
		)

	;; === Clojure (SPC m)
	(my-global-leader-def
		:keymaps '(clojure-ts-mode-map
							 clojure-ts-clojurescript-mode-map
							 clojure-ts-clojurec-mode-map
							 clojure-ts-clojuredart-mode-map)
		"m" '(:ignore t :wk "Clojure")
		"m i" '(cider-jack-in :wk "cider jack-in")
		"m c" '(:ignore t :wk "cider connect")
		"m c c" '(cider-connect-clj&cljs :wk "connect clj&cljs")
		"m c j" '(cider-connect-clj :wk "connect clj")
		"m c s" '(cider-connect-cljs :wk "connect cljs")
		"m q" '(cider-quit :wk "cider quit")
		"l F" '(my-clojure-lsp-clear-cache-and-restart :wk "lsp clear cache/restart")
		)

	;; === Clojure (,)
	(my-local-leader-def
		:keymaps '(clojure-ts-mode-map
							 clojure-ts-clojurescript-mode-map
							 clojure-ts-clojurec-mode-map
							 clojure-ts-clojuredart-mode-map)
		"e" '(:ignore t :wk "Eval")
		"e e" '(cider-eval-last-sexp :wk "eval last sexp")
		"e f" '(cider-eval-dwim :wk "eval dwim")
		"e b" '(cider-eval-buffer :wk "eval bufer")
		"e n" '(cider-eval-ns-form :wk "eval ns form")
		"i" '(cider-insert-defun-in-repl :wk "insert to repl")
		"n" '(:ignore t :wk "Namespace")
		"n r" '(cider-ns-refresh :wk "cider ns refresh")
		"n s" '(cider-repl-set-ns :wk "cider ns set")
		"t" '(cider-switch-to-repl-buffer :wk "cider switch to repl"))

	;; === Emacs Lisp (,)
	(my-local-leader-def
		:keymaps '(emacs-lisp-mode-map
							 lisp-interaction-mode-map)
		"'" '(ielm :wk "ielm")
		"e" '(:ignore t :wk "Eval")
		"e e" '(eval-last-sexp :wk "eval last sexp")
		"e f" '(eval-defun :wk "eval defun")
		"e b" '(eval-buffer :wk "eval buffer")
		"i" '(my-insert-time :wk "insert time")
		"p" '(my-package-built-in-p :wk "check package built-in")
		)

	;; === Emacs Lispの便利ヘルプ
	(general-define-key
	 :keymaps 'emacs-lisp-mode-map
	 :states '(normal)
	 "K" 'helpful-at-point)

	;; === Markdown
	(my-local-leader-def
		:keymaps '(gfm-mode-map)
		"," '(markdown-toggle-gfm-checkbox :wk "toggle checkbox"))

	;; === Flymakeのエラージャンプ
	(general-define-key
	 :states '(normal)
	 "] ]" '(flymake-goto-next-error :wk "goto next error")
	 "[ [" '(flymake-goto-prev-error :wk "goto prev error"))

	;; === ミニバッファでパスならスラッシュまで削除
	(general-define-key
	 :states '(insert)
	 :keymaps '(minibuffer-mode-map)
	 "C-w" 'backward-kill-sexp)

	;; === isearch(C-sまたは/)中に単語削除はM-eの後にC-DELを押すしかない
	;; 基本はC-hで納得しよう

	;; === Copilot Chatでshift+enterで送信
	(general-define-key
	 :keymaps 'copilot-chat-org-prompt-mode-map
	 "S-<return>" 'copilot-chat-prompt-send))

;;====================================================================
;; 設定・色の細かいカスタマイズ
;;====================================================================

;; customizeメニューから変更した場合自動で追記・更新される
;; パッケージ固有の設定変数はuse-packageの:customで設定する方が宣言的で良い
;; 色の調整はここにまとめておいた方が見通しが良い(現在： catppuccin-macchiato を前提に調整)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages nil)
 '(package-vc-selected-packages
	 '((claude-code-ide :url
											"https://github.com/manzaltu/claude-code-ide.el")
		 (copilot :url "https://github.com/copilot-emacs/copilot.el"
							:branch "main")))
 '(safe-local-variable-directories '("/Users/shota.508/Studist/teachme_eboshigara/")))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(diff-added ((t (:background "#3e4b4c"))))
 '(diff-refine-added ((t (:background "#586e5e"))))
 '(diff-refine-removed ((t (:background "#744d5f"))))
 '(diff-removed ((t (:background "#4c3a4c"))))
 '(ediff-current-diff-A ((t (:extend t :background "#4c3a4c"))))
 '(ediff-current-diff-B ((t (:extend t :background "#3e4b4c"))))
 '(ediff-current-diff-C ((t (:extend t :background "#4c4540"))))
 '(ediff-fine-diff-A ((t (:background "#744d5f"))))
 '(ediff-fine-diff-B ((t (:background "#586e5e"))))
 '(ediff-fine-diff-C ((t (:background "#746355"))))
 '(font-lock-comment-delimiter-face ((t (:foreground "#5ab5b0"))))
 '(font-lock-comment-face ((t (:foreground "#5ab5b0"))))
 '(match ((t (:background "#eed49f" :foreground "#1e2030"))))
 '(show-paren-match ((t (:background "#8aadf4" :foreground "#1e2030" :weight bold))))
 '(show-paren-mismatch ((t (:background "#ed8796" :foreground "#1e2030" :weight bold))))
 '(trailing-whitespace ((t (:background "#ed8796" :foreground "#ed8796")))))

;; === ローディング終了メッセージ
(message "[%s] %s" (my-display-time) "init.el loaded!")

;;; init.el ends here
