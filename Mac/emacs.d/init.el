;;; init.el --- メイン設定ファイル -*- lexical-binding: t; -*-

;;; Commentary:
;; Emacsの設定ファイル

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

;; === pareditでカーソル以降をそのレベルで閉じるまで削除
(defun my-paredit-kill-to-end ()
  "Kill to the end of the current sexp."
  (interactive)
  (let ((end (save-excursion (paredit-close-parenthesis) (point))))
    (kill-region (point) (- end 1))))

;; === カーソル下のシンボルが組み込みのパッケージかどうかチェック
(defun my-package-built-in-p (symbol)
  "Check if SYMBOL is a built-in package."
  (interactive (list (or (symbol-at-point)
                         (intern (read-string "Package: ")))))
  (message "[%s] built-in: %s" symbol (when (package-built-in-p symbol) t)))

;;====================================================================
(message "[%s] %s" (my-display-time) "init.el loading...")

;;===== TIPS / Rules of use-package ==================================
;; 空行は入れないように統一する
;; 組み込みは:ensure nil, 外部は:ensure t
;; :after 依存関係, 1行でOK。これ以外は基本キーワード後に開業
;; :init パッケージのロード前に実行
;; :custom defcustom変数はこちらで設定 (setq x y)のsetqを除いた形式
;; :hook パッケージに関連するフックをコンスセル形式で設定 (x . y)
;; 末尾が-hookならそのまま、そうでなければ-hookを付けたものが使われる。関数側は#'をつけない
;; :config 通常通りの処理をまとめる意味(グローバルスコープになる)
;; :vc GitHubやCodebergなどから直接インストールする場合に利用
;;====================================================================

;;====================================================================
;; シェル環境変数をDockからの起動でも利用する
;;====================================================================

(use-package exec-path-from-shell
  :ensure t
  ;; NOTE: PATH以外も欲しい場合はcustomで指定
  :config
  (exec-path-from-shell-initialize))

;;====================================================================
;; Emacs標準機能の設定
;;====================================================================

;; ===== キーバインド
;; === MacのCommandをMetaキーに
(setq mac-command-modifier 'meta)

;; === C-hをBackspaceに、C-;をC-hに割り当て
(define-key key-translation-map (kbd "C-h") (kbd "DEL"))
(define-key key-translation-map (kbd "C-;") (kbd "C-h"))

;; ===== システム・Emacsコア連携
;; === 自分で配置したEmacsのソースコードへの参照を追加
;; 利用しているEmacsバージョンによって適宜ソースコードはダウンロード必要
(use-package find-func
  :ensure nil
  :config
  (setq find-function-C-source-directory
        (concat "~/Documents/OSS/emacs/emacs-" emacs-version "/src")))

;; === デーモン起動 (シェルの`e'コマンドから使う)
;; === `e'コマンド (~/.zshrcなどに追加)
;; # emacsclient
;; e() {
;;   if emacsclient --eval "t" > /dev/null 2>&1; then
;;     emacsclient -n "$@"
;;   else
;;     emacs "$@" &
;;   fi
;; }
(use-package server
  :ensure nil
  :config
  (unless (server-running-p)
    (server-start)))

;; ===== ファイル管理
;; === バックアップファイルを作成しない
(setq make-backup-files nil)
(setq backup-inhibited t)
(setq create-lockfiles nil)

;; === ダイアログでのファイルオープンは使わない
(setq use-file-dialog nil)

;; === 削除したファイルをゴミ箱に移動
(setq delete-by-moving-to-trash t)

;; === 他プロセスの編集をバッファに反映
(global-auto-revert-mode t)

;; === シンボリックリンクを常に質問なしで開く
(setq vc-follow-symlinks t)

;; === 長い行を含むファイルの最適化
(global-so-long-mode t)

;; ===== 編集体験の向上
;; === ミニバッファでのyes/noの聞かれ方をy/nにする
(setq use-short-answers t)

;; === インデントの基本をスペースに変更
(setq-default indent-tabs-mode nil
              tab-width 2
              standard-indent 2)

;; === 対応括弧を補完
(electric-pair-mode t)

;; === ファイル履歴を保存
(use-package recentf
  :ensure nil
  :custom
  (recentf-max-saved-items 50)
  (recentf-mode t))

;; === コマンドの履歴を保存
(savehist-mode t)

;; === ウィンドウの状態を保持
(winner-mode t)

;; === ウィンドウの分割方向の閾値を広げる
(setq split-width-threshold 140)

;; === 末尾のスペースやタブを可視化
(defun my-turn-on-show-trailing-ws ()
  "Visualize trailing whitespace buffer-locally."
  (setq-local show-trailing-whitespace t))
(add-hook 'prog-mode-hook #'my-turn-on-show-trailing-ws)
(add-hook 'text-mode-hook #'my-turn-on-show-trailing-ws)

;; === which-keyのディレイ
(use-package which-key
  :ensure nil
  :custom
  (which-key-mode t)
  (which-key-idle-delay 0.3)
  (which-key-idle-secondary-delay 0))

;; ==== モード別設定
;; === zshファイルを開いたときにshell-script-modeを有効に
(add-to-list 'auto-mode-alist '("\\zshrc\\'" . shell-script-mode))
(add-to-list 'auto-mode-alist '("\\zprofile\\'" . shell-script-mode))
(add-to-list 'auto-mode-alist '("\\zshenv\\'" . shell-script-mode))

;;====================================================================
;; バッファの表示方法についての設定 (display-buffer-alist)
;;====================================================================

;; === 後で追加したものが優先される

;; diredを開くときは分割して表示される
(add-to-list 'display-buffer-alist
             '((derived-mode . dired-mode)
               (display-buffer-pop-up-window
                display-buffer-use-some-window)
               (side . right)
               (window-width . 0.5)))

;; dired上でdiredを開くときは同じウィンドウで開く
;; RETでその場で、S-RETで別のウィンドウで開く
(add-to-list 'display-buffer-alist
             '((lambda (buffer-name action)
                 (and (with-current-buffer buffer-name (derived-mode-p 'dired-mode))
                      (with-current-buffer (window-buffer (selected-window))
                        (derived-mode-p 'dired-mode))))
               (display-buffer-same-window)))

;; ielmは分割して開く
(add-to-list 'display-buffer-alist
             '("\\*ielm\\*"
               (display-buffer-pop-up-window
                display-buffer-use-some-window)
               (side . right)
               (window-width . 0.5)))

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
(defun my-switch-ime (input-source)
  "Emacsにフォーカスが当たったときにmacSKK.asciiに切り替える(macismコマンド必要)"
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
  (isearch-mode-end-hook . skk-isearch-mode-cleanup)
  (evil-normal-state-entry-hook . skk-latin-mode-on)
  (text-mode-hook . my-turn-on-skk)
  (prog-mode-hook . my-turn-on-skk)
  (eat-mode-hook . my-turn-on-skk)
  :bind
  ("C-x j" . skk-mode)
  ("C-j" . skk-kakutei)
  :config
  (defun my-turn-on-skk ()
    "skk-modeを有効にして、英字モードにする"
    (skk-mode t)
    (skk-latin-mode-on)))

;;====================================================================
;; UIと外観 (フォントとテーマ)
;;====================================================================

;; === 現在行を強調表示
(global-hl-line-mode t)

;; === 行間を少し広げる
(setq-default line-spacing 0.07)

;; === 行番号を表示
(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(add-hook 'text-mode-hook #'display-line-numbers-mode)

;; === カーソル位置の列番号をモードラインに表示
(column-number-mode t)

;; === tree-sittterによる色付けmax
(use-package treesit
  :ensure nil
  :custom
  (treesit-font-lock-level 4))

;; === フォント設定
(set-face-attribute 'default nil :font "Source Han Code JP-14")
(set-face-attribute 'fixed-pitch nil :font "Source Han Code JP-14")
(set-face-attribute 'variable-pitch nil :font "Source Han Code JP-14")

;; === nerd iconsを利用
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
  (doom-modeline-modeal t)
  (doom-modeline-major-mode-icon nil))

;; === ダッシュボード
(use-package dashboard
  :ensure t
  :custom
  (dashboard-startup-banner 'logo)
  (dashboard-center-content t)
  (dashboard-vertically-center-content t)
  (dashboard-items '((recents . 5)
                     (bookmarks . 5)
                     (projects . 5)))
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
        ("," . nil)))

;; === evilの便利なキーバインド追加
(use-package evil-collection
  :ensure t
  :after evil
  :custom
  (evil-collection-setup-minibuffer t)
  (evil-collection-key-blacklist '("C-j" "C-k"))
  :config
  (evil-collection-init))

;; === fdでESCできるように
(use-package evil-escape
  :ensure t
  :after evil
  :custom
  (evil-escape-mode t)
  :config
  ;; 0引数要件があるのでlambdaでラップ
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
  (dired-listing-switches "-alhG --time-style=long-iso"))

;;====================================================================
;; ミニバッファ内での検索・候補選択
;;====================================================================

;; === 便利な統合コマンドの提供 (consult)
(use-package consult
  :ensure t
  :custom
  (consult-async-min-input 2)
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
  (completion-styles '(orderless basic partial-completion))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion))
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
  :custom
  (eglot-events-buffer-config '(:size nil :format full))
  (eglot-autoshutdown t)
  (eglot-connect-timeout 120))

;; === スニペット・テンプレート (tmpel)
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
;; ターミナル (eat)
;;====================================================================

;; === eat
(use-package eat
  :ensure t
  :vc (:url "http://codeberg.org/akib/emacs-eat" :rev :newest)
  :custom
  (eat-enable-shell-prompt-annotation nil)
  :config
  (setq process-adaptive-read-buffering nil)
  )
;; TODO: SKKのread-only問題解消
;; TODO: poppoerの導入

;;====================================================================
;; Git操作 (magit・diff-hl・vc)
;;====================================================================

;; === magit
(use-package magit
  :ensure t)

;; === フリンジに差分を強調表示 (diff-hl)
(use-package diff-hl
  :ensure t
  :custom
  (global-diff-hl-mode t)
  :config
  (global-diff-hl-mode)
  (add-hook 'magit-pre-refresh-hook #'diff-hl-magit-pre-refresh)
  (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh)
  (diff-hl-flydiff-mode t))

;; === magitをgit-deltaを使って高速化しつつ見やすくする
;; bat, deltaのインストール
;; catppuccinのbat, delta用のテーマ設定がそれぞれ必要
(use-package magit-delta
  :ensure t
  :hook (magit-mode . magit-delta-mode)
  :custom
  (magit-delta-default-dark-theme "Catppuccin Macchiato"))

;;====================================================================
;; ワークスペース (perspective.el)
;;====================================================================

(use-package perspective
  :ensure t
  :init
  (setq persp-suppress-no-prefix-key-warning t)
  (persp-mode)
  :custom
  (persp-state-default-file "~/.cache/emacs/workspace-default")
  (persp-sort 'created)
  )

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
  :ensure t)

(use-package cider
  :ensure t
  :hook (clojure-ts-mode . cider-mode)
  :custom
  (cider-repl-buffer-size-limit 10000)
  (cider-font-lock-dynamically '(macro core function var deprecated))
  )

;; TODO: ciderとclojure-lsp(eglot)の補完は使いながら調整
;; TODO: ciderの便利機能や設定も使いながら獲得(portalなども)

;; 構造的編集(Paredit)
(use-package paredit
  :ensure t
  :bind (:map paredit-mode-map
              ("C-j" . nil))
  :hook ((emacs-lisp-mode . paredit-mode)
         (lisp-interaction-mode . paredit-mode)
         (clojure-ts-mode . paredit-mode)))

;; Javaライブラリのジャンプ時などに
(use-package jarchive
  :ensure t
  :after eglot
  :config
  (jarchive-setup)
  (setq eglot-extend-to-xref t))

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

  :bind (:map markdown-mode-map
              ("C-c C-e" . markdown-do)))

;;====================================================================
;; Claude Code連携
;;====================================================================

(use-package claude-code-ide
  :ensure t
  :vc (:url "http://github.com/manzaltu/claude-code-ide.el" :rev :newest)
  :custom
  ;; skkとの相性の関係からvtermではなくeatを使用
  (claude-code-ide-terminal-backend 'eat)
  :config
  (claude-code-ide-emacs-tools-setup))

;;====================================================================
;; GitHub Copilot連携
;;====================================================================

(use-package copilot
  :ensure t
  :vc (:url "https://github.com/copilot-emacs/copilot.el" :rev :newest :branch "main")
  :hook
  (prog-mode . copilot-mode)
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
  (add-to-list 'copilot-indentation-alist '(emacs-lisp-mode  2))
  )

(use-package copilot-chat
  :ensure t)

;;====================================================================
;; Dockerコンテナ内開発ワークフロー
;;====================================================================

(use-package dockerfile-mode
  :ensure t
  :mode ("Dockerfile\\'" . dockerfile-mode))

(use-package yaml-mode
  :ensure t
  :mode ("\\.ya?ml\\'" . yaml-mode))

(use-package docker
  :ensure t
  :custom
  (docker-container-columns
   '(
     (:name "Names" :width 30 :template "{{ json .Names }}" :sort nil :format nil)
     (:name "Status" :width 30 :template "{{ json .Status }}" :sort nil :format nil)
     (:name "Ports" :width 30 :template "{{ json .Ports }}" :sort nil :format nil)
     ))
  (docker-container-default-sort-key '("Names")))

(add-to-list 'tramp-remote-path 'tramp-own-remote-path)
(setq tramp-default-method "docker")
(setq tramp-default-remote-shell "/bin/bash")

;; コンテナをシェルで起動しておく
;; あとは、find-fileから/docker:コンテナ名:/path/to/fileで接続すればlspもうまく動作

;;====================================================================
;; DB接続
;;====================================================================

(use-package sql
  :ensure nil
  :config
  (setq sql-mode-hook
        `(lambda ()
           (sql-indent-enable)
           (sql-highlight-postgres-keywords)))

  (setq sql-postgres-login-params nil)
  (setq sql-connection-alist
        '((eboshigara-postgres (sql-product 'postgres)
                               (sql-database (concat "postgresql://"
                                                     "root"
                                                     ":" (my-read-1password "eboshigara_dev_db")
                                                     "@localhost"
                                                     ":54320"
                                                     "/eboshigara_dev")))))
  (defun my-read-1password (name)
    "1Passwordからパスワードを取得する"
    (let ((password (shell-command-to-string
                     (format "op read op://Private/%s/password" name))))
      (string-trim password)))
  )

;;====================================================================
;; Format On Save設定の集約
;;====================================================================

;; === Emacs Lisp
(defun my-format-emacs-lisp ()
  (interactive)
  (save-excursion
    (indent-region (point-min) (point-max)))
  (message "[elisp] formatted."))

;; === Clojure
(defun my-format-clojure ()
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
    (clojure-ts-mode . (:lsp my-format-clojure))))

(defun my-format-try (formatter)
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
  "現在のメジャーモードに応じたフォーマッタを実行する"
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

  ;; === SPC リーダーキー定義
  (general-create-definer my-global-leader-def
    :states '(normal visual)
    :keymaps 'override
    :prefix "SPC")

  ;; === メジャーモード用のローカルリーダーを作成
  (general-create-definer my-local-leader-def
    :states '(normal)
    :keymaps 'override
    :prefix ",")

  ;; === モーション系(g)
  (general-create-definer my-motion-leader-def
    :states '(normal visual)
    :keymaps 'override
    :prefix "g")

  ;; === グローバルなSPCキーバインド
  (my-global-leader-def
    ;; (SPC) M-x
    "SPC" '(execute-extended-command :wk "M-x")

    ;; (;) コメント
    ";" '(evil-commentary-line :wk "comment")

    ;; (t) トグル
    "t" '(:ignore t :wk "Toggle")
    "t l" '(toggle-truncate-lines :wk "truncate line")
    "t f" '(flymake-mode :wk "toggle flymake")

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

    ;; (b) バッファ操作/ブックマーク
    "b" '(:ignore t :wk "Buffers/Bookmark")
    "b b" '(persp-switch-to-buffer :wk "buffer switch")
    "b d" '(kill-current-buffer :wk "buffer delete")
    "b h" '(dashboard-open :wk "dashboard")
    "b l" '(consult-bookmark :wk "bookmark list")
    "b s" '(bookmark-set :wk "bookmark set")
    "b d" '(bookmark-delete :wk "bookmark delete")

    ;; (s) 検索
    "s" '(:ignore t :wk "Search")
    "s s" '(consult-line :wk "search in buffer")
    "s p" '(consult-ripgrep :wk "search in project")
    "s f" '(consult-flymake :wk "search flymake")

    ;; (p) プロジェクト管理
    "p" '(:ignore t :wk "Project/Package")
    "p p" '(project-switch-project :wk "project switch")

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
    "a c" '(copilot-chat :wk "Copilot chat") ; 補完はM-/でサジェスト
    "a i" '(claude-code-ide-menu :wk "Claude Code IDE")
    )

  ;; === ジャンプなど (g系)
  (my-motion-leader-def
    "n" '(diff-hl-next-hunk :wk "next change")
    "p" '(diff-hl-previous-hunk :wk "prev change")
    "t" '(persp-next :wk "next workspace")
    "T" '(persp-prev :wk "prev workspace")
    )

  ;; === LSP (eglot) 操作
  (my-local-leader-def
    :keymaps '(eglot-mode-map)
    "l" '(:ignore t :wk "LSP")
    "l r" '(eglot-rename :wk "rename symbol")
    "l a" '(eglot-code-actions :wk "code actions")
    "l f" '(eglot-format :wk "format")
    )

  ;; === Lisp系の編集操作
  (my-local-leader-def
    :keymaps '(emacs-lisp-mode-map
               lisp-interaction-mode-map
               clojure-ts-mode-map)
    "s" '(paredit-forward-slurp-sexp :wk "slurp forward")
    "S" '(paredit-backward-slurp-sexp :wk "slurp backward")
    "b" '(paredit-forward-barf-sexp :wk "barf forward")
    "B" '(paredit-backward-barf-sexp :wk "barf backward")
    "r" '(paredit-raise-sexp :wk "raise sexp")
    "w" '(:ignore t :wk "wrap")
    "w (" '(paredit-wrap-round :wk "wrap ()")
    "w [" '(paredit-wrap-square :wk "wrap []")
    "w {" '(paredit-wrap-curly :wk "wrap {}")
    "w \"" '(paredit-meta-doublequote :wk "wrap \"\"")
    "k" '(my-paredit-kill-to-end :wk "kill to sexp end")
    "h" '(eldoc :wk "eldoc")
    )

  ;; === Clojure
  (my-local-leader-def
    :keymaps '(clojure-ts-mode-map)
    ", i" '(cider-juck-in :wk "cider juck-in")
    ", c" '(cider-connect :wk "cider connect" )
    ", q" '(cider-quit :wk "cider quit")
    ", e" '(cider-eval-dwim :wk "cider eval dwim")
    ", r" '(cider-ns-refresh :wk "cider ns refresh")
    ;; ここはEmacs Lispとあわせるか
    )

  ;; === Emacs Lisp
  (my-local-leader-def
    :keymaps '(emacs-lisp-mode-map
               lisp-interaction-mode-map)
    "'" '(ielm :wk "ielm")
    "e" '(:ignore t :wk "Eval")
    "e e" '(eval-last-sexp :wk "eval last sexp")
    "e f" '(eval-defun :wk "eval defun")
    "e b" '(eval-buffer :wk "eval buffer")
    "i" '(my-insert-time :wk "insert time")
    "p" '(my-package-built-in-p :wk "check package built-in"))

  ;; === Markdown
  (my-local-leader-def
    :keymaps '(gfm-mode-map)
    "," '(markdown-toggle-gfm-checkbox :wk "toggle checkbox"))

  ;; === ミニバッファでパスならスラッシュまで削除
  (general-define-key
   :states '(insert)
   :keymaps 'minibuffer-mode-map
   "C-w" 'backward-kill-sexp)

  ;; === Emacs Lispの便利ヘルプ
  (general-define-key
   :keymaps 'emacs-lisp-mode-map
   :states '(normal)
   "K" 'helpful-at-point)

  ;; === Flymakeのエラージャンプ
  (general-define-key
   :states '(normal)
   "] ]" '(flymake-goto-next-error :wk "goto next error")
   "[ [" '(flymake-goto-prev-error :wk "goto prev error"))

  ;; === Copilot Chatでshift+enterで送信
  (general-define-key
   :keymaps 'copilot-chat-org-prompt-mode-map
   "S-<return>" 'copilot-chat-prompt-send))

;;====================================================================
;; 設定・色の細かいカスタマイズ
;;====================================================================

;; customizeメニューから変更した場合自動で追記・更新される
;; パッケージ固有の設定変数はuse-packageの:customで設定する方が宣言的で良い
;; 色の調整はここにまとめておいた方が見通しが良い

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(diff-added ((t (:background "#4b5d55"))))
 '(diff-refine-added ((t (:background "#658168"))))
 '(diff-refine-removed ((t (:background "#895768"))))
 '(diff-removed ((t (:background "#604456"))))
 '(font-lock-comment-delimiter-face ((t (:foreground "#5ab5b0"))))
 '(font-lock-comment-face ((t (:foreground "#5ab5b0"))))
 '(match ((t (:background "#eed49f" :foreground "#1e2030"))))
 '(show-paren-match ((t (:background "#8aadf4" :foreground "#1e2030" :weight bold))))
 '(show-paren-mismatch ((t (:background "#ed8796" :foreground "#1e2030" :weight bold))))
 '(trailing-whitespace ((t (:background "#ed8796" :foreground "#ed8796")))))

(message "[%s] %s" (my-display-time) "init.el loaded!")
