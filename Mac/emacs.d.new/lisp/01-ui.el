;;; 01-ui.el --- UI/外観設定 -*- lexical-binding: t; -*-

;;; Commentary:
;; テーマ、フォント、モードライン、表示設定

;;; Code:

;; 行番号・列番号
(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(add-hook 'text-mode-hook #'display-line-numbers-mode)
(column-number-mode t)

;; カーソル行ハイライト
(global-hl-line-mode t)

;; フォント
(defvar my-font-default "Source Han Code JP")
(defvar my-font-term "UDEV Gothic 35NF")  ; vterm/TUI用
(set-face-attribute 'default nil :font my-font-default :height 130)
(set-fontset-font t 'han (font-spec :family my-font-default) nil 'prepend)
;; unicode範囲外文字用フォントでガタつかないようにする
(dolist (char '(#x00B7 #x2722 #x2733 #x2736 #x273B #x273D))
  (set-fontset-font t char (font-spec :family "JuliaMono")))

;; nerd-icons (初回: M-x nerd-icons-install-fonts)
(use-package nerd-icons :ensure t)

;; catppuccin テーマ
(use-package catppuccin-theme
  :ensure t
  :custom (catppuccin-flavor 'macchiato)
  :config (load-theme 'catppuccin t))

;; doom-modeline
(use-package doom-modeline
  :ensure t
  :custom
  (doom-modeline-mode t)
  (doom-modeline-icon nil)
  (doom-modeline-major-mode-icon nil)
  (doom-modeline-modal nil)
  (doom-modeline-buffer-file-name-style 'file-name)
  (doom-modeline-buffer-encoding nil))

;; rainbow-delimiters - 括弧の色分け
(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

;; hl-todo - TODO/FIXME等をハイライト
(use-package hl-todo
  :ensure t
  :custom
  (hl-todo-keyword-faces '(("TODO" warning bold)
                           ("NOTE" ansi-color-cyan bold)
                           ("XXX" error bold)))
  :config (global-hl-todo-mode t))

;; colorful-mode - カラーコードを実際の色で表示
(use-package colorful-mode
  :ensure t
  :custom (colorful-use-prefix t)
  :config (global-colorful-mode t))

;; dashboard - 起動画面
(use-package dashboard
  :ensure t
  :custom
  (dashboard-startup-banner 'logo)
  (dashboard-center-content t)
  (dashboard-vertically-center-content t)
  (dashboard-items '((recents . 10) (bookmarks . 5) (projects . 5)))
  :config (dashboard-setup-startup-hook))

;; ultra-scroll - 滑らかスクロール
(use-package ultra-scroll
  :ensure t
  :vc (:url "https://github.com/jdtsmith/ultra-scroll")
  :init (setq scroll-conservatively 3 scroll-margin 0)
  :config (ultra-scroll-mode t))

;; 色のカスタマイズ (catppuccin-macchiato ベース)
(custom-set-faces
 '(mode-line ((t (:background "#1e2030"))))
 '(show-paren-match ((t (:background "#8aadf4" :foreground "#1e2030" :weight bold))))
 '(font-lock-comment-face ((t (:foreground "#5ab5b0"))))
 '(font-lock-comment-delimiter-face ((t (:foreground "#5ab5b0"))))
 '(trailing-whitespace ((t (:background "#ed8796")))))

(provide '01-ui)

;;; 01-ui.el ends here
