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
 '(package-selected-packages
   '(bufferfile cape catppuccin-theme cider claude-code-ide
                clojure-ts-mode colorful-mode copilot corfu dashboard
                ddskk diff-hl docker doom-modeline eglot-tempel
                eldoc-box elfeed embark-consult evil-anzu
                evil-collection evil-commentary evil-escape
                evil-goggles evil-surround exec-path-from-shell forge
                format-all general github-dark-vscode-theme
                github-theme groovy-mode helpful hl-todo jarchive
                marginalia multi-vterm nerd-icons-corfu orderless
                osx-dictionary puni rainbow-delimiters restclient
                terraform-mode treemacs-evil treemacs-nerd-icons
                treemacs-perspective ultra-scroll undo-fu
                undo-fu-session vertico vundo wat-ts-mode wgrep
                zig-mode))
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
 '(cursor ((t (:background "#cdd6f4"))))
 '(diff-added ((t (:background "#394545"))))
 '(diff-refine-added ((t (:background "#546d5c"))))
 '(diff-refine-removed ((t (:background "#734a5f"))))
 '(diff-removed ((t (:background "#493446"))))
 '(ediff-current-diff-A ((t (:extend t :background "#493446"))))
 '(ediff-current-diff-B ((t (:extend t :background "#394545"))))
 '(ediff-current-diff-C ((t (:extend t :background "#4a4548"))))
 '(ediff-fine-diff-A ((t (:background "#734a5f"))))
 '(ediff-fine-diff-B ((t (:background "#546d5c"))))
 '(ediff-fine-diff-C ((t (:background "#766c62"))))
 '(font-lock-comment-delimiter-face ((t (:foreground "#5ab5b0"))))
 '(font-lock-comment-face ((t (:foreground "#5ab5b0"))))
 '(match ((t (:background "#f9e2af" :foreground "#181825"))))
 '(mode-line ((t (:background "#181825"))))
 '(show-paren-match ((t (:background "#89b4fa" :foreground "#181825" :weight bold))))
 '(show-paren-mismatch ((t (:background "#f38ba8" :foreground "#181825" :weight bold))))
 '(trailing-whitespace ((t (:background "#f38ba8" :foreground "#f38ba8")))))

;;; init.el ends here
