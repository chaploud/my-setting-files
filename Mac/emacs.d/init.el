;;; init.el --- メイン設定ファイル -*- lexical-binding: t; -*-

;;; Commentary:
;; パッケージ管理の初期化と各設定ファイルの読み込み
;;
;; 外部依存:
;; - emacs-plus@30 (https://github.com/d12frosted/homebrew-emacs-plus)
;; - gcc, libgccjit (native-comp用)

;;; Code:

;; パッケージ設定
(require 'package)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")
                         ("melpa" . "https://melpa.org/packages/")))
(setq package-check-signature nil) ;; TODO: 本当は署名検証させたい
(package-initialize)

(require 'use-package)

;; 設定ファイルに読み込み (順序あり)
(add-to-list 'load-path (expand-file-name "my-lisp" user-emacs-directory))
(require '00-basic)
(require '01-japanese)
(require '02-ui)
(require '03-evil)
(require '04-git)
(require '05-tools)
(require '06-terminal)
(require '07-lsp)
(require '08-fuzzy-finder)
(require '09-completion)
(require '10-ai)
(require '11-clojure)
(require '12-languages)
(require '98-utilities)
(require '99-keybindings)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages nil)
 '(package-vc-selected-packages
   '((wat-ts-mode :url "https://github.com/nverno/wat-ts-mode" :branch
                  "master")
     (claude-code-ide :url
                      "https://github.com/manzaltu/claude-code-ide.el")
     (copilot :url "https://github.com/copilot-emacs/copilot.el"
              :branch "main")))
 '(safe-local-variable-directories
   '("/Users/shota.508/Documents/OSS/melpa/" "/Users/shota.508/myskill/")))

(defvar my-base-bg "#24273a")
(defvar my-base-fg "#cad3f5")
(defvar my-black "#1e2030")
(defvar my-blue "#8aadf4")
(defvar my-error "#ed8796")
(defvar my-success "#a6da95")
(defvar my-warning "#eed49f")
(defvar my-comment "#5ab5b0")
(defvar my-error-dim-1 "#744d5f")
(defvar my-error-dim-2 "#4c3a4c")
(defvar my-success-dim-1 "#586e5e")
(defvar my-success-dim-2 "#3e4b4c")
(defvar my-warning-dim-1 "#746355")
(defvar my-warning-dim-2 "#4c4540")

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(consult-file ((t)))
 `(cursor ((t (:background ,my-base-fg))))
 `(diff-added ((t (:background ,my-success-dim-2))))
 `(diff-refine-added ((t (:background ,my-success-dim-1))))
 `(diff-refine-removed ((t (:background  ,my-error-dim-1))))
 `(diff-removed ((t (:background ,my-error-dim-2))))
 `(ediff-current-diff-A ((t (:extend t :background ,my-error-dim-2))))
 `(ediff-current-diff-B ((t (:extend t :background ,my-success-dim-2))))
 `(ediff-current-diff-C ((t (:extend t :background ,my-warning-dim-2))))
 `(ediff-fine-diff-A ((t (:background ,my-error-dim-1))))
 `(ediff-fine-diff-B ((t (:background ,my-success-dim-1))))
 `(ediff-fine-diff-C ((t (:background ,my-warning-dim-1))))
 `(flymake-error-echo-at-eol
   ((t (:background ,my-error-dim-1 :foreground ,my-base-fg :box (:line-width (6 . -1) :color ,my-error-dim-1) :height 0.85))))
 `(flymake-note-echo-at-eol
   ((t (:background ,my-success-dim-1 :foreground ,my-base-fg :box (:line-width (6 . -1) :color ,my-success-dim-1) :height 0.85))))
 `(flymake-warning-echo-at-eol
   ((t (:background ,my-warning-dim-1 :foreground ,my-base-fg :box (:line-width (6 . -1) :color ,my-warning-dim-1) :height 0.85))))
 `(font-lock-comment-delimiter-face ((t (:foreground ,my-comment))))
 `(font-lock-comment-face ((t (:foreground ,my-comment))))
 `(match ((t (:background ,my-warning :foreground ,my-black))))
 `(mode-line ((t (:background ,my-black))))
 `(show-paren-match ((t (:background ,my-blue :foreground ,my-black :weight bold))))
 `(show-paren-mismatch ((t (:background ,my-error :foreground ,my-black :weight bold))))
 `(trailing-whitespace ((t (:background ,my-error :foreground ,my-error)))))

;;; init.el ends here
