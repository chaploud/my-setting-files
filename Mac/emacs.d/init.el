;;; init.el --- メイン設定ファイル -*- lexical-binding: t; -*-

;====================================================================
; パッケージ管理 (use-packageのブートストラップ)
;====================================================================

;; package.elの初期化とアーカイブ設定
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)
(add-to-list 'package-archives '("elpa" . "https://elpa.gnu.org/packages/") t)
(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

;; === use-packageのensureは必ず行う
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

(use-package gnu-elpa-keyring-update)

;====================================================================
; Emacs標準機能の設定
;====================================================================
;; === MacのCommandをMetaキーに
(setq mac-command-modifier 'meta)

;; === y/nにする
(setq use-short-answers t)

;; === バックアップファイルを作成しない
(setq make-backup-files nil)
(setq backup-inhibited nil)
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
(defun turn-on-show-trailing-ws ()
  (setq-local show-trailing-whitespace t))
(add-hook 'prog-mode-hook #'turn-on-show-trailing-ws)
(add-hook 'text-mode-hook #'turn-on-show-trailing-ws)

;; === which-keyのディレイ
(which-key-mode +1)
(setq which-key-idle-delay 0.3
      which-key-idle-secondary-delay 0)

;; === GNU版のlsを使う
(setq insert-directory-program "gls")

;====================================================================
; UIと外観 (フォントとテーマ)
;====================================================================
(use-package dashboard
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-startup-banner 'logo)
  (setq dashboard-center-content t)
  (setq dashboard-vertically-center-content t)
  (setq dashboard-items '((recents . 5)
                          (bookmarks . 5)
                          (projects . 5))))

;; === シェル環境変数をDockからの起動でも利用する
(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize))

;; === nerd iconsを利用
(use-package nerd-icons)

;; === フォント設定
(set-face-attribute 'default nil :font "Source Han Code JP-14")
(set-face-attribute 'fixed-pitch nil :font "Source Han Code JP-14")
(set-face-attribute 'variable-pitch nil :font "Source Han Code JP-14")

;; === カラーテーマ
(use-package catppuccin-theme
  :init
  (setq catppuccin-flavor 'macchiato) ; latte/frappe/macchiato
  :config
  (load-theme 'catppuccin t)
  )

;; === doom-modeline
(use-package doom-modeline
  :init
  (doom-modeline-mode +1)
  :config
  (setq doom-modeline-modal t)
  (setq doom-modeline-major-mode-icon nil)
  )

;; === フォントキャッシュ最適化
(setq inhibit-compacting-font-caches t)

;; === TODOハイライト
(use-package hl-todo
  :config
  (setq hl-todo-keyword-faces
        `(("TODO" warning bold)
          ("NOTE" ansi-color-cyan bold)
          ("XXX" error bold)))
  (global-hl-todo-mode +1)
  )

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;====================================================================
; EvilによるVimキーバインド
;====================================================================

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
  (evil-escape-mode +1)
  )

;; === 囲み系の操作
(use-package evil-surround
  :after evil
  :config
  (global-evil-surround-mode +1)
  )

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

;====================================================================
; 日本語入力
;====================================================================

;; === ddskk
(defun switch-ime (input-source)
  (call-process "macism" nil 0 nil input-source))
(add-function :after after-focus-change-function
              (lambda ()
                (if (frame-focus-state)
                    (switch-ime "com.apple.keylayout.ABC")
                  (switch-ime "net.mtgto.inputmethod.macSKK.hiragana"))))

(define-key global-map (kbd "C-j") nil)
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

(defun turn-on-skk ()
  (skk-mode +1)
  (skk-latin-mode-on))

(add-hook 'text-mode-hook #'turn-on-skk)
(add-hook 'prog-mode-hook #'turn-on-skk)

;====================================================================
; 補完システム(ミニバッファ)
;====================================================================

;; === 補完候補を垂直に表示するUI (vertico)
(use-package vertico
  :init
  (vertico-mode +1)
  :custom
  (vertico-cycle t) ; 末尾から先頭の候補にサイクル
  (vertico-count 12) ; 表示する候補の最大数 & 固定高さ
  :config
  (setq vertico-resize nil)
  )

;; === 柔軟な絞り込みスタイル (orderless)
(use-package orderless
  :custom
  (completion-styles '(orderless basic partial-completion))
  (completio--category-override '((file (styles basic partial-completion)))))

;; === 補完候補に注釈を追加 (marginalia)
(use-package marginalia
  :after vertico
  :init
  (marginalia-mode +1))

;====================================================================
; 補完システム(バッファ内)
;====================================================================

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
  (setq tab-always-indent 'complete)
  )

;; === スニペット・テンプレート (tmpel)
(use-package tempel
  :init
  (defun tempel-setup-capf ()
    (setq-local completion-at-point-functions
                (cons #'tempel-expand
                      completion-at-point-functions)))
  :hook
  ((conf-mode . tempel-setup-capf)
   (prog-mode . tempel-setup-capf)
   (text-mode . tempel-setup-capf)))

;; === 補完ソースの拡張 (cape)
(use-package cape
  :config
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  )

;; === 補完のアイコン
(use-package nerd-icons-corfu
  :after (corfu nerd-icons)
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

;====================================================================
; 開発ツール (LSP, ターミナル, Git)
;====================================================================

;; === lsp (eglot)
;; TODO 特定のメジャーモードでeglotをonにする
;; TODO 特定のメジャーモードでflymakeをonにする

;; === vterm
(use-package vterm
  :commands vterm
  :config
  (setq vterm-max-scrollback 10000))

(use-package vterm-toggle
  :config
  (define-key global-map (kbd "C-'") #'vterm-toggle))

;; === magit
(use-package magit)

;; === フリンジに差分を強調表示
(use-package diff-hl
  :config
  (global-diff-hl-mode)
  (add-hook 'magit-pre-refresh-hook #'diff-hl-magit-pre-refresh)
  (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh)
  )

;====================================================================
; ワークスペース (perspective.el)
;====================================================================
(use-package perspective
  :init
  (setq persp-suppress-no-prefix-key-warning t)
  (persp-mode))

;====================================================================
; Swipe検索 (consult)
;====================================================================
(use-package consult
  :hook (completion-list-mode . consult-preview-at-point-mode)
  )

;====================================================================
; Clojure/ClojureScript/ClojureDart
;====================================================================
(use-package clojure-ts-mode)
(use-package cider
  :hook (clojure-ts-mode . cider-mode))

;====================================================================
; キーバインド (general.el)
;====================================================================

;; === ユーティリティ関数
(defun open-user-init ()
  (interactive)
  (find-file user-init-file))

;; use-packageと:generalの組み合わせで色々できる
(use-package general
  :after evil
  :config
  (general-evil-setup)
  (general-auto-unbind-keys)

  ;; SPC リーダーキー定義
  (general-create-definer leader-def
    :states '(normal visual)
    :keymap 'override
    :prefix "SPC")

  ;; メジャーモード用のローカルリーダーを作成
  (general-create-definer local-leader-def
    :states '(normal visual)
    :keymap 'override
    :prefix ",")

  ;; モーション系(g)
  (general-create-definer motion-leader-def
    :states '(normal visual)
    :keymap 'override
    :prefix "g")

  ;; 基本的なSPCキーバインド
  (leader-def
    "SPC" '(execute-extended-command :wk "M-x")

    ;; (q) 終了操作
    "q" '(:ignore t :wk "Quit")
    "qq" '(save-buffers-kill-terminal :wk "quit")
    "qr" '(restart-emacs :wk "restart")

    ;; (f) ファイル操作
    "f" '(:ignore t :wk "Files")
    "ff" '(find-file :wk "file find")
    "fr" '(recentf-open :wk "file recent")
    "fs" '(save-buffer :wk "file save")
    "fi" '(open-user-init :wk "init.el")
    "ft" '(project-dired :wk "dired")

    ;; (b) バッファ操作
    "b" '(:ignore t :wk "Buffers")
    "bb" '(switch-to-buffer :wk "buffer switch")
    "bd" '(kill-current-buffer :wk "buffer delete")
    "bh" '(dashboard-open :wk "dashboard")

    ;; (g) Git/GoTo
    "g" '(:ignore t :wk "Git/GoTo")
    "gs" '(magit-status-quick :wk "git status")
    "gl" '(magit-log-current :wk "git log")
    "gd" '(vc-diff :wk "git diff")
    "gn" '(flymake-goto-next-error :wk "goto next error")
    "gp" '(flymake-goto-prev-error :wk "goto prev error")

    ;; (d) diff, debug
    "d" '(:ignore t :wk "Diff/Debug")
    "dd" '(diff-hl-show-hunk :wk "diff")

    ;; (p) プロジェクト管理
    "p" '(:ignore t :wk "Project")
    "pp" '(project-switch-project :wk "project switch")
    "pf" '(project-find-file :wk "project find")

    ;; (s) 検索
    "s" '(:ignore t :wk "Search")
    "ss" '(consult-line :wk "search in buffer")
    "sp" '(consult-ripgrep :wk "search in project")

    ;; (w) ワークスペース
    "w" '(:ignore t :wk "Workspace")
    "ww" '(persp-switch :wk "workspace switch")
    "wr" '(persp-rename :wk "workspace rename")
    "wd" '(persp-kill :wk "workspace kill")

    ;; (;) コメント
    ";" '(evil-commentary-line :wk "comment")
    )

  ;; メジャーモード
  ;; (local-leader-def
  ;;   :keymap 'override
  ;;   "d" 'diff-hl-show-hunk
  ;;   )

  ;; モーション
  (motion-leader-def
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
 '(package-selected-packages nil))
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
