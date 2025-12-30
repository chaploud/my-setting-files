;;; 02-ui.el --- UI設定 -*- lexical-binding: t; -*-

;;; Commentary:
;;; Code:

(global-hl-line-mode t) ; カーソル行ハイライト
(column-number-mode t)  ; 列番号をモードラインに表示
(add-hook 'prog-mode-hook #'display-line-numbers-mode) ; 行番号 (ソースコード)
(add-hook 'text-mode-hook #'display-line-numbers-mode) ; 行番号 (テキストファイル)

;; フォント
(defvar my-font "Source Han Code JP")
(set-face-attribute 'default nil :family my-font :height 130)

;; カラーテーマ
(use-package catppuccin-theme
  :ensure t
  :custom
  (catppuccin-flavor 'macchiato)
  :config
  (load-theme 'catppuccin t))

;; アイコンフォント
;; M-x nerd-icons-install-fonts
(use-package nerd-icons
  :ensure t)

;; モードライン
(use-package doom-modeline
  :ensure t
  :custom
  (doom-modeline-mode t)
  (doom-modeline-modal nil)                         ; evilのモード非表示
  (doom-modeline-major-mode-icon nil)               ; メジャーモードアイコン非表示
  (doom-modeline-buffer-encoding nil)               ; LF/UTF-8など非表示
  (doom-modeline-buffer-file-name-style 'file-name) ; ファイル名のみ
  :config
  ;; anzuのカウントを左グループの右端に表示
  (doom-modeline-def-modeline 'my-main
    '(eldoc bar window-state workspace-name window-number modals follow buffer-info remote-host buffer-position word-count parrot selection-info matches)
    '(compilation objed-state misc-info project-name persp-name battery grip irc mu4e gnus github debug repl lsp minor-modes input-method indent-info buffer-encoding major-mode process vcs check time))
  (doom-modeline-set-modeline 'my-main t))

;; ダッシュボード
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

;; なめらかなホイールスクロール
(use-package ultra-scroll
  :ensure t
  :vc (:url "https://github.com/jdtsmith/ultra-scroll")
  :init
  (setq scroll-conservatively 3
        scroll-margin 0)
  :config
  (ultra-scroll-mode t))

;; 括弧の階層的色分け
(use-package rainbow-delimiters
  :ensure t
  :hook
  (prog-mode . rainbow-delimiters-mode))

;; TODOなどをハイライト
(use-package hl-todo
  :ensure t
  :custom
  (hl-todo-keyword-faces
   `(("TODO" warning bold)
     ("NOTE" ansi-color-cyan bold)
     ("XXX" error bold)))
  (global-hl-todo-mode t))

;; カラーコードに色を表示
(use-package colorful-mode
  :ensure t
  :custom
  (colorful-use-prefix t)
  (global-colorful-mode t))

(provide '02-ui)
;;; 02-ui.el ends here
