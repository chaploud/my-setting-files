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

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(consult-file ((t)))
 '(cursor ((t (:background "#cad3f5"))))
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
 '(flymake-end-of-line-diagnostics-face ((t (:box nil :height 0.85))))
 '(font-lock-comment-delimiter-face ((t (:foreground "#5ab5b0"))))
 '(font-lock-comment-face ((t (:foreground "#5ab5b0"))))
 '(match ((t (:background "#eed49f" :foreground "#1e2030"))))
 '(mode-line ((t (:background "#1e2030"))))
 '(show-paren-match ((t (:background "#8aadf4" :foreground "#1e2030" :weight bold))))
 '(show-paren-mismatch ((t (:background "#ed8796" :foreground "#1e2030" :weight bold))))
 '(trailing-whitespace ((t (:background "#ed8796" :foreground "#ed8796")))))

;;; init.el ends here
