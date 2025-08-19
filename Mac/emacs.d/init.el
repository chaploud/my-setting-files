;;; init.el --- メイン設定ファイル -*- lexical-binding: t; -*-

;;====================================================================
;; パッケージ管理 (use-packageのブートストラップ)
;;====================================================================

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
  :config
  (exec-path-from-shell-initialize))

;;====================================================================
;; Emacs標準機能の設定
;;====================================================================

;; === MacのCommandをMetaキーに
(setq mac-command-modifier 'meta)

;; === y/nにする
(setq use-short-answers t)

;; === バックアップファイルを作成しない
(setq make-backup-files nil)
(setq backup-inhibited t)
(setq create-lockfiles nil)

;; === ビープ音無効化
(setq ring-bell-function 'ignore)

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
(setq-default indent-tabs-mode nil)

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
  :config
  (setq skk-server-host "127.0.0.1")
  (setq skk-server-portnum 1178)
  (setq skk-dcomp-activate t)
  (setq skk-egg-like-newline t)
  (setq skk-delete-implies-kakutei nil)
  (setq skk-use-color-cursor nil)
  (setq skk-show-candidates-nth-henkan-char 3)

  :hook
  (evil-normal-state-entry-hook
   . (lambda ()
       (when (bound-and-true-p skk-mode)
         (skk-latin-mode-on))))
  )

(defun my-turn-on-skk ()
  (skk-mode +1)
  (skk-latin-mode-on))

(add-hook 'text-mode-hook #'my-turn-on-skk)
(add-hook 'prog-mode-hook #'my-turn-on-skk)

;;====================================================================
;; EvilによるVimキーバインド
;;====================================================================

;; === evilによるVimキーバインドのエミュレート
(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)

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
  (setq evil-symbol-word-search t) ; ひとかたまりで検索
  )

;; === evilの便利なキーバインド追加
(use-package evil-collection
  :after evil
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
  (setq evil-goggles-enable-delete nil)

  :config
  (evil-goggles-mode +1)
  (evil-goggles-use-diff-refine-faces)
  (setq evil-goggles-duration 0.100))

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
;; ミニバッファ内での検索・候補選択 
;;====================================================================

;; === 便利な統合コマンドの提供 (consult)
(use-package consult
  :hook (completion-list-mode . consult-preview-at-point-mode))

;; === 補完候補を垂直に表示するUI (vertico)
(use-package vertico
  :init
  (vertico-mode +1)

  :custom
  (vertico-cycle t)                  ; 末尾から先頭の候補にサイクル
  (vertico-count 12)                 ; 表示する候補の最大数 & 固定高さ

  :config
  (setq vertico-resize nil))

;; === 柔軟な絞り込みスタイル (orderless)
(use-package orderless
  :custom
  (completion-styles '(orderless basic partial-completion))
  (completio-category-override '((file (styles basic partial-completion))
                                 (eglot (styles orderless)))))

;; === 補完候補に注釈を追加 (marginalia)
(use-package marginalia
  :after vertico
  :init
  (marginalia-mode +1))

;;====================================================================
;; バッファ(エディタ)内のポップアップ補完
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

  :config
  (setq tab-always-indent 'complete))

;; === 補完のアイコン
(use-package nerd-icons-corfu
  :after (corfu nerd-icons)
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

;; === スニペット・テンプレート (tmpel)
(use-package tempel
  :config
  (defun my-tempel-setup-capf ()
    (setq-local completion-at-point-functions
                (cons #'tempel-expand completion-at-point-functions)))
  :hook
  ((prog-mode . my-tempel-setup-capf)
   (text-mode . my-tempel-setup-capf)))

;; === 補完ソースの統合・拡張 (cape)
(use-package cape
  :config
  (defun my-super-capf-with-lsp ()
    (setq-local completion-at-point-functions
                (list (cape-capf-super
                       #'eglot-completion-at-point
                       #'tempel-expand
                       #'cape-dabbrev
                       #'cape-file))))
  :hook
  ((eglot-managed-mode . my-super-capf-with-lsp)))

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
  (persp-mode))

;;====================================================================
;; LSP (eglot)
;;====================================================================

;; === lsp (eglot)
;; TODO 特定のメジャーモードでeglotをonにする
;; TODO 特定のメジャーモードでflymakeをonにする
;; TODO flymake-consultを活用する
(use-package eglot)

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

;; 構造的編集(Paredit)
(use-package paredit
  :bind (:map paredit-mode-map
              ("C-j" . nil))
  :hook ((emacs-lisp-mode . paredit-mode)
         (lisp-interaction-mode . paredit-mode)
         (clojure-ts-mode . paredit-mode)))

;;====================================================================
;; TODO Claude Code連携
;;====================================================================
;;====================================================================
;; TODO GitHub Copilot連携
;;====================================================================
;;====================================================================
;; TODO Dockerコンテナ内開発ワークフロー
;;====================================================================

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
    "qq" '(save-buffers-kill-terminal :wk "quit")
    "qr" '(restart-emacs :wk "restart")

    ;; (f) ファイル操作
    "f" '(:ignore t :wk "Files")
    "ff" '(find-file :wk "file find")
    "fr" '(recentf-open :wk "file recent")
    "fp" '(project-find-file :wk "find in project")
    "fs" '(save-buffer :wk "file save")
    "fi" '(my-open-user-init :wk "init.el")
    "ft" '(project-dired :wk "dired")

    ;; (b) バッファ操作
    "b" '(:ignore t :wk "Buffers")
    "bb" '(switch-to-buffer :wk "buffer switch")
    "bd" '(kill-current-buffer :wk "buffer delete")
    "bh" '(dashboard-open :wk "dashboard")

    ;; (g) Git/ジャンプ
    "g" '(:ignore t :wk "Git/GoTo")
    "gs" '(magit-status-quick :wk "git status")
    "gl" '(magit-log-current :wk "git log")
    "gd" '(vc-diff :wk "git diff")
    "gn" '(flymake-goto-next-error :wk "goto next error")
    "gp" '(flymake-goto-prev-error :wk "goto prev error")

    ;; (d) 差分/デバッグ
    "d" '(:ignore t :wk "Diff/Debug")
    "dd" '(diff-hl-show-hunk :wk "diff")

    ;; (p) プロジェクト管理
    "p" '(:ignore t :wk "Project")
    "pp" '(project-switch-project :wk "project switch")

    ;; (s) 検索
    "s" '(:ignore t :wk "Search")
    "ss" '(consult-line :wk "search in buffer")
    "sp" '(consult-ripgrep :wk "search in project")

    ;; (w) ワークスペース/ウィンドウ操作
    "w" '(:ignore t :wk "Workspace/Window")
    "ww" '(persp-switch :wk "workspace switch")
    "wr" '(persp-rename :wk "workspace rename")
    "wd" '(persp-kill :wk "workspace kill")
    "wu" '(winner-undo :wk "window undo")

    ;; (;) コメント
    ";" '(evil-commentary-line :wk "comment")
    )

  ;; === Lisp系の編集操作
  (my-local-leader-def
    :keymaps '(emacs-lisp-mode-map
               lisp-interaction-mode-map
               clojure-ts-mode-map)
    "s" 'paredit-forward-slurp-sexp
    "S" 'paredit-backward-slurp-sexp
    "b" 'paredit-forward-barf-sexp
    "B" 'paredit-backward-barf-sexp
    "r" 'paredit-raise-sexp
    "w" '(:ignore t :wk "wrap")
    "w(" '(paredit-wrap-round :wk "wrap ()")
    "w[" '(paredit-wrap-square :wk "wrap []")
    "w{" '(paredit-wrap-curly :wk "wrap {}")
    "w\"" '(paredit-meta-doublequote :wk "wrap \"\"")
    )

  ;; === Emacs Lisp
  (my-local-leader-def
    :keymaps '(emacs-lisp-mode-map
               lisp-interaction-mode-map)
    "e" 'eval-defun)

  ;; ジャンプ
  (my-motion-leader-def
    "n" '(diff-hl-next-hunk :wk "next change")
    "p" '(diff-hl-previous-hunk :wk "prev change")
    "t" '(persp-next :wk "next workspace")
    "T" '(persp-prev :wk "prev workspace")
    )
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
 '(font-lock-comment-face ((t (:foreground "#5ab5b0")))))
