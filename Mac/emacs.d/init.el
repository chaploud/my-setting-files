;;; init.el --- メイン設定ファイル -*- lexical-binding: t; -*-

;;====================================================================
;; パッケージ管理 (use-packageのブートストラップ)
;;===================================================================

;; package.elの初期化とアーカイブ設定
(setq package-check-signature nil)
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)
(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

;; === use-packageのensureは必ず行う
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

;; === シェル環境変数をDockからの起動でも利用する
(use-package exec-path-from-shell
  ;; NOTE: PATH以外も欲しい場合はcustomで指定
  :config
  (exec-path-from-shell-initialize))

;;====================================================================
;; Emacs標準機能の設定
;;====================================================================

;; === 自分で配置したEmacsのソースコードへの参照を追加
(setq find-function-C-source-directory
      (concat "~/Documents/OSS/emacs/emacs-" emacs-version "/src"))

;; === MacのCommandをMetaキーに
(setq mac-command-modifier 'meta)

;; === y/nにする
(setq use-short-answers t)

;; === バックアップファイルを作成しない
(setq make-backup-files nil)
(setq backup-inhibited t)
(setq create-lockfiles nil)

;; === フレームのタイトル
(setq-default frame-title-format "Emacs")
(setq-default ns-use-proxy-icon nil)

;; === デーモン起動
(use-package server
  :config
  (unless (server-running-p)
    (server-start)))

;; === ファイル履歴を保存
(setq recentf-max-saved-items 100)
(recentf-mode +1)

;; === コマンドの履歴を保存
(savehist-mode +1)

;; === ウィンドウの状態を保持
(winner-mode +1)

;; === 対応括弧を補完
(electric-pair-mode +1)

;; === 現在行を強調表示
(global-hl-line-mode +1)

;; === 他プロセスの編集をバッファに反映
(global-auto-revert-mode +1)

;; === 行間を少し広げる
(setq-default line-spacing 0.07)

;; === 行番号を表示
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'text-mode-hook 'display-line-numbers-mode)

;; === カーソル位置の列番号をモードラインに表示
(column-number-mode +1)

;; === 削除したファイルをゴミ箱に移動
(setq delete-by-moving-to-trash t)

;; === インデントの基本をスペースに変更
(setq-default indent-tabs-mode nil
              tab-width 2
              standard-indent 2)

;; === 長い行を含むファイルの最適化
(global-so-long-mode +1)

;; === 末尾のスペースやタブを可視化
(defun my-turn-on-show-trailing-ws ()
  (setq-local show-trailing-whitespace t))
(add-hook 'prog-mode-hook #'my-turn-on-show-trailing-ws)
(add-hook 'text-mode-hook #'my-turn-on-show-trailing-ws)

;; === which-keyのディレイ
(which-key-mode +1)
(setq which-key-idle-delay 0.3
      which-key-idle-secondary-delay 0)

;;====================================================================
;; 日本語入力
;;====================================================================

;; === ddskk
(defun my-switch-ime (input-source)
  (call-process "macism" nil 0 nil input-source))
(add-function :after after-focus-change-function
              (lambda ()
                (if (frame-focus-state)
                    (my-switch-ime "com.apple.keylayout.ABC")
                  (my-switch-ime "net.mtgto.inputmethod.macSKK.hiragana"))))

(use-package ddskk
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
  (evil-normal-state-entry-hook
   . (lambda ()
       (when (bound-and-true-p skk-mode))))
  (isearch-mode-hook . skk-isearch-mode-setup)
  (isearch-mode-end-hook . skk-isearch-mode-cleanup)
  :bind ("C-j" . skk-kakutei))

(defun my-turn-on-skk ()
  (skk-mode +1)
  (skk-latin-mode-on))

(add-hook 'text-mode-hook #'my-turn-on-skk)
(add-hook 'prog-mode-hook #'my-turn-on-skk)

;;====================================================================
;; UIと外観 (フォントとテーマ)
;;====================================================================

;; === フォント設定
(set-face-attribute 'default nil :font "Source Han Code JP-14")
(set-face-attribute 'fixed-pitch nil :font "Source Han Code JP-14")
(set-face-attribute 'variable-pitch nil :font "Source Han Code JP-14")

;; === nerd iconsを利用
(use-package nerd-icons)

;; === カラーテーマ
(use-package catppuccin-theme
  :init
  (setq catppuccin-flavor 'macchiato) ; latte/frappe/macchiato

  :config
  (load-theme 'catppuccin t))

;; === カーソルの色をオーバーライド
(set-cursor-color "#cad3f5")

;; === 対応カッコを色付け表示
(use-package rainbow-delimiters
  :hook
  (prog-mode . rainbow-delimiters-mode))

;; === doom-modeline
(use-package doom-modeline
  :init
  (doom-modeline-mode +1)

  :config
  (setq doom-modeline-modal t)
  (setq doom-modeline-major-mode-icon nil))

;; === ダッシュボード
(use-package dashboard
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-startup-banner 'logo)
  (setq dashboard-center-content t)
  (setq dashboard-vertically-center-content t)
  (setq dashboard-items '((recents . 5)
                          (bookmarks . 5)
                          (projects . 5))))

;; === TODOハイライト
(use-package hl-todo
  :config
  (setq hl-todo-keyword-faces
        `(("TODO" warning bold)
          ("NOTE" ansi-color-cyan bold)
          ("XXX" error bold)))
  (global-hl-todo-mode +1))

;;====================================================================
;; EvilによるVimキーバインド
;;====================================================================

(use-package undo-fu)
(use-package undo-fu-session
  :after undo-fu
  :config
  (undo-fu-session-global-mode +1))

;; === evilによるVimキーバインドのエミュレート
(use-package evil
  :after (undo-fu undo-fu-session)
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-undo-system 'undo-fu)

  :config
  (evil-mode +1)
  ;; Emacsキーバインドも一部使う
  (define-key evil-insert-state-map (kbd "C-f") nil)
  (define-key evil-insert-state-map (kbd "C-b") nil)
  (define-key evil-insert-state-map (kbd "C-n") nil)
  (define-key evil-insert-state-map (kbd "C-p") nil)
  (define-key evil-insert-state-map (kbd "C-a") nil)
  (define-key evil-insert-state-map (kbd "C-e") nil)
  (define-key evil-insert-state-map (kbd "C-k") nil)
  (define-key evil-insert-state-map (kbd "C-d") nil)
  (define-key evil-insert-state-map (kbd "C-y") nil)
  (define-key key-translation-map (kbd "C-h") (kbd "DEL"))
  (define-key key-translation-map (kbd "C-;") (kbd "C-h"))
  (define-key evil-motion-state-map (kbd ",") nil)
  (define-key evil-insert-state-map (kbd "C-S-w") #'backward-kill-sexp)
  (setq evil-symbol-word-search t)      ; ひとかたまりで検索
  (setq evil-shift-width 2)
  )

;; === evilの便利なキーバインド追加
(use-package evil-collection
  :after evil
  :custom (evil-collection-setup-minibuffer t)
  :config
  (evil-collection-init)
  (setq evil-collection-key-blacklist '("C-j" "C-k"))
  )

;; === fdでESCできるように
(use-package evil-escape
  :after evil
  :config
  (evil-escape-mode +1))

;; === 囲み系の操作
(use-package evil-surround
  :after evil
  :config
  (global-evil-surround-mode +1))

;; === 編集操作をハイライト
(use-package evil-goggles
  :after evil
  :init
  ;; 削除系はハイライトせずとも分かるし反映が遅れるとストレスなのでオフ
  (setq evil-goggles-enable-delete nil)
  (setq evil-goggles-enable-change nil)

  :config
  (evil-goggles-mode +1)
  ;; (evil-goggles-use-diff-refine-faces)
  (setq evil-goggles-duration 0.200))

;; === 検索ヒット件数を表示
(use-package evil-anzu
  :after evil
  :config
  (global-anzu-mode +1))

;; === コメントアウト
(use-package evil-commentary
  :config
  (evil-commentary-mode +1))

;;====================================================================
;; ファイルツリー (dired-subtree)
;;====================================================================

(use-package dired-subtree)
(setq insert-directory-program "gls") ;; GNU版lsを使う
(setq dired-dwim-target t)
(setq dired-listing-switches "-alhG --time-style=long-iso")

;;====================================================================
;; バッファの表示方法についての設定 (display-buffer-alist)
;;====================================================================

;; === 後で追加したものが優先される

(add-to-list 'display-buffer-alist
             '((derived-mode . dired-mode)
               (display-buffer-pop-up-window
                display-buffer-use-some-window)
               (side . right)
               (window-width . 0.5)))

(add-to-list 'display-buffer-alist
             '((lambda (buffer-name action)
                 (and (with-current-buffer buffer-name (derived-mode-p 'dired-mode))
                      (with-current-buffer (window-buffer (selected-window))
                        (derived-mode-p 'dired-mode))))
               (display-buffer-same-window)))
;; RETでその場で、S-RETで別のウィンドウで開く

;;====================================================================
;; ミニバッファ内での検索・候補選択
;;====================================================================

(use-package wgrep
  :config
  (setq wgrep-auto-save-buffer t))

;; === 便利な統合コマンドの提供 (consult)
(use-package consult
  :hook (completion-list-mode . consult-preview-at-point-mode))

;; === 補完候補を垂直に表示するUI (vertico)
(use-package vertico
  :init
  (vertico-mode +1)

  :custom
  (vertico-cycle t)                  ; 末尾から先頭の候補にサイクル
  (vertico-count 15)                 ; 表示する候補の最大数 & 固定高さ

  :config
  (setq vertico-resize nil))

;; === 柔軟な絞り込みスタイル (orderless)
(use-package orderless
  :custom
  (completion-styles '(orderless basic partial-completion))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion))))
  (orderless-matching-styles '(orderless-literal orderless-flex orderless-regexp)))

;; === 補完候補に注釈を追加 (marginalia)
(use-package marginalia
  :after vertico
  :init
  (marginalia-mode +1))

(use-package embark
  :bind
  (("C-." . embark-act)
   ("C-," . embark-export)))

;; === 候補に対するアクション (embark-consult)
(use-package embark-consult
  :after (embark consult)
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;;====================================================================
;; バッファ内のインライン/ポップアップ補完
;;====================================================================

;; === バッファ内補完のUIフロントエンド (corfu)
(use-package corfu
  :init
  (global-corfu-mode +1)
  (corfu-popupinfo-mode +1)
  (corfu-history-mode +1)

  :custom
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
  :after (corfu nerd-icons)
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

;;====================================================================
;; LSP (eglot)
;;====================================================================

;; === lsp (eglot)
;; TODO 特定のメジャーモードでeglotをonにする
;; TODO 特定のメジャーモードでflymakeをonにする
;; TODO eglotのステータスが有効な時特有のキーバインドを行う
;; TODO (xref, flymake, eldoc, eglot-rename, eglot-code-actions)
;; TODO flymake-consultを活用する
;; TODO .lsp/config.ednとの連携
(use-package eglot
  :ensure nil
  :hook (clojure-ts-mode . eglot-ensure)
  :config
  (setq eglot-events-buffer-config '(:size nil :format full)
        eglot-autoshutdown t
        eglot-connect-timeout 120))

;; === スニペット・テンプレート (tmpel)
(use-package tempel)

(use-package eglot-tempel
  :init (eglot-tempel-mode +1))

;; === 補完ソースの統合・拡張 (cape)
(use-package cape
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

;; === vterm
(use-package vterm
  :commands vterm
  :config
  (setq vterm-max-scrollback 10000))

;; === 賢くvtermをトグル
(use-package vterm-toggle
  :config
  (define-key global-map (kbd "C-'") #'vterm-toggle))

;;====================================================================
;; Git操作 (magit・diff-hl・vc)
;;====================================================================

;; === magit
(use-package magit
  :config
  (setq magit-diff-refine-hunk t)
  (setq magit-diff-refine-ignore-whitespace nil))

;; === フリンジに差分を強調表示 (diff-hl)
(use-package diff-hl
  :config
  (global-diff-hl-mode)
  (add-hook 'magit-pre-refresh-hook #'diff-hl-magit-pre-refresh)
  (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh)
  (diff-hl-flydiff-mode +1))

;;====================================================================
;; ワークスペース (perspective.el)
;;====================================================================

(use-package perspective
  :init
  (setq persp-suppress-no-prefix-key-warning t)
  (persp-mode)
  :custom
  (persp-state-default-file "~/.cache/emacs/workspace-default"))

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
(use-package clojure-ts-mode)
(use-package cider
  :hook (clojure-ts-mode . cider-mode))

;; TODO: ciderとclojure-lsp(eglot)の補完は使いながら調整
;; TODO: ciderの便利機能や設定も使いながら獲得(portalなども)

;; 構造的編集(Paredit)
(use-package paredit
  :bind (:map paredit-mode-map
              ("C-j" . nil))
  :hook ((emacs-lisp-mode . paredit-mode)
         (lisp-interaction-mode . paredit-mode)
         (clojure-ts-mode . paredit-mode)))

;; Javaライブラリのジャンプ時などに
(use-package jarchive
  :after eglot
  :config
  (jarchive-setup)
  (setq eglot-extend-to-xref t))

;;====================================================================
;; Markdown
;;====================================================================
(use-package markdown-mode
  :mode ("\\.md\\'" . gfm-mode)
  :init
  (setq markdown-command '("pandoc" "--from=markdown" "--to=html5"))

  :config
  (setq-default markdown-fontify-code-blocks-natively t)

  :bind (:map markdown-mode-map
              ("C-c C-e" . markdown-do)))

;;====================================================================
;; Claude Code連携
;;====================================================================

(use-package claude-code-ide
  :vc (:url "http://github.com/manzaltu/claude-code-ide.el" :rev :newest)
  :config
  (claude-code-ide-emacs-tools-setup))

;;====================================================================
;; GitHub Copilot連携
;;====================================================================

(use-package copilot
  :vc (:url "https://github.com/copilot-emacs/copilot.el" :rev :newest :branch "main")
  :hook (prog-mode . copilot-mode)
  :bind (:map copilot-completion-map
              ("C-<tab>" . copilot-accept-completion))
  :custom (copilot-max-char 1000000) ; 最大文字数を増やす
  :config
  (add-to-list 'copilot-indentation-alist '(prog-mode  2))
  (add-to-list 'copilot-indentation-alist '(org-mode  2))
  (add-to-list 'copilot-indentation-alist '(text-mode  2))
  (add-to-list 'copilot-indentation-alist '(emacs-lisp-mode  2))
  )

(use-package copilot-chat)

;;====================================================================
;; Dockerコンテナ内開発ワークフロー
;;====================================================================

(use-package dockerfile-mode
  :mode ("Dockerfile\\'" . dockerfile-mode))

(use-package yaml-mode
  :mode ("\\.ya?ml\\'" . yaml-mode))

(use-package docker
  :custom
  (docker-container-columns
   '(
     (:name "Names" :width 30 :template "{{ json .Names }}" :sort nil :format nil)
     (:name "Status" :width 30 :template "{{ json .Status }}" :sort nil :format nil)
     (:name "Ports" :width 30 :template "{{ json .Ports }}" :sort nil :format nil)
     ))
  (docker-container-default-sort-key '("Names"))
  )

(add-to-list 'tramp-remote-path 'tramp-own-remote-path)
(setq tramp-default-method "docker")
(setq tramp-verbose 6) ; 初期デバッグ用(デフォルトは3)
;; コンテナをシェルで起動しておく
;; あとは、find-fileから/docker:コンテナ名:/path/to/fileで接続すればlspもうまく動作

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

;;====================================================================
;; キーバインド (general.el)
;;====================================================================

;; === ユーティリティ関数
(defun my-open-user-init ()
  (interactive)
  (find-file user-init-file))

;; use-packageと:generalの組み合わせで色々できる
(use-package general
  :after (evil evil-collection)
  :config
  (general-evil-setup)
  (general-auto-unbind-keys)

  (general-define-key
   :states '(insert)
   :keymaps 'minibuffer-mode-map
   "C-w" 'backward-kill-sexp)

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
    "SPC" '(execute-extended-command :wk "M-x")

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

    ;; (b) バッファ操作
    "b" '(:ignore t :wk "Buffers")
    "b b" '(persp-switch-to-buffer :wk "buffer switch")
    "b d" '(kill-current-buffer :wk "buffer delete")
    "b h" '(dashboard-open :wk "dashboard")

    ;; (m) ブックマーク
    "m" '(:ignore t :wk "Bookmark")
    "m m" '(consult-bookmark :wk "bookmark list")
    "m s" '(bookmark-set :wk "bookmark set")
    "m d" '(bookmark-delete :wk "bookmark delete")
    "m r" '(bookmark-rename :wk "bookmark rename")

    ;; (g) Git/ジャンプ
    "g" '(:ignore t :wk "Git/GoTo")
    "g s" '(magit-status-quick :wk "git status")
    "g l" '(magit-log-current :wk "git log")
    "g d" '(vc-diff :wk "git diff")
    "g n" '(flymake-goto-next-error :wk "goto next error")
    "g p" '(flymake-goto-prev-error :wk "goto prev error")

    ;; (d) 差分/デバッグ/Docker
    "d" '(:ignore t :wk "Diff/Debug/Docker")
    "d d" '(diff-hl-show-hunk :wk "diff")
    "d c" '(docker-containers :wk "docker containers")

    ;; (p) プロジェクト管理
    "p" '(:ignore t :wk "Project")
    "p p" '(project-switch-project :wk "project switch")

    ;; (s) 検索
    "s" '(:ignore t :wk "Search")
    "s s" '(consult-line :wk "search in buffer")
    "s p" '(consult-ripgrep :wk "search in project")

    ;; (w) ワークスペース/ウィンドウ操作
    "w" '(:ignore t :wk "Workspace/Window")
    "w w" '(persp-switch :wk "workspace switch")
    "w r" '(persp-rename :wk "workspace rename")
    "w d" '(persp-kill :wk "workspace kill")
    "w s" '(persp-state-save :wk "workspace save")
    "w l" '(persp-state-load :wk "workspace load")
    "w u" '(winner-undo :wk "window undo")

    ;; (;) コメント
    ";" '(evil-commentary-line :wk "comment")

    ;; (t) トグル
    "t" '(:ignore t :wk "Toggle")
    "t l" '(toggle-truncate-lines :wk "truncate line")

    ;; (a) 生成AI系
    "a" '(:ignore t :wk "AI")
    "a i" '(claude-code-ide-menu :wk "Claude Code IDE")
    "a s" '(copilot-complete :wk "Copilot suggest")
    "a c" '(copilot-chat :wk "Copilot chat")
    )

  ;; === ジャンプなど
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
    "h" '(eldoc :wk "eldoc")
    )

  ;; === Emacs Lisp
  (my-local-leader-def
    :keymaps '(emacs-lisp-mode-map
               lisp-interaction-mode-map)
    "e" '(eval-defun :wk "eval defun"))

  ;; === Markdown
  (my-local-leader-def
    :keymaps '(gfm-mode-map)
    "," '(markdown-toggle-gfm-checkbox :wk "toggle checkbox"))
  )

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
 '(evil-goggles-change-face ((t (:inherit diff-refine-removed))))
 '(evil-goggles-delete-face ((t (:inherit diff-refine-removed))))
 '(evil-goggles-paste-face ((t (:inherit diff-refine-added))))
 '(evil-goggles-undo-redo-add-face ((t (:inherit diff-refine-added))))
 '(evil-goggles-undo-redo-change-face ((t (:inherit diff-refine-changed))))
 '(evil-goggles-undo-redo-remove-face ((t (:inherit diff-refine-removed))))
 '(evil-goggles-yank-face ((t (:inherit diff-refine-changed))))
 '(font-lock-comment-delimiter-face ((t (:foreground "#5ab5b0"))))
 '(font-lock-comment-face ((t (:foreground "#5ab5b0"))))
 '(match ((t (:background "#eed49f" :foreground "#1e2030"))))
 '(show-paren-match ((t (:background "#8aadf4" :foreground "#1e2030" :weight bold))))
 '(show-paren-mismatch ((t (:background "#ed8796" :foreground "#1e2030" :weight bold))))
 '(trailing-whitespace ((t (:background "#ed8796" :foreground "#ed8796")))))
