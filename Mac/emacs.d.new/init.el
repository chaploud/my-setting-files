;;; init.el --- メイン設定ファイル -*- lexical-binding: t; -*-

;;; Commentary:
;; パッケージ管理の初期化と各設定ファイルの読み込み

;; 最終更新: 2025-12-28

;; 依存関係

;; === 全般 ===
;; emacs-plus@30.2 (https://github.com/d12frosted/homebrew-emacs-plus)
;; gcc@15.2.0 (https://formulae.brew.sh/formula/gcc)
;; libgccjit@15.2.0 (https://formulae.brew.sh/formula/libgccjit)

;; === 00-basic.el ===
;; `~/.zsrhc'
;; # eコマンドでサっとEmacsでファイルを開く
;; e() {
;;   if emacsclient --eval "t" > /dev/null 2>&1; then
;;     emacsclient -n "$@"
;;   else
;;     emacs "$@" &
;;   fi
;; }
;;
;; brew install pass
;; brew install gpg

;; === 01-japanese.el ===
;; macSKK (https://github.com/mtgto/macSKK)
;; yaskkserv2 (https://github.com/wachikun/yaskkserv2)
;; macism (https://github.com/laishulu/macism)

;;; Code:

;; パッケージ設定
(require 'package)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")
                         ("melpa" . "https://melpa.org/packages/")))
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
(require '06-termianl)
(require '07-lsp)
(require '08-fuzzy-finder)
(require '09-completion)
(require '10-ai)
(require '11-clojure)
(require '12-languages)
(require '98-utilities)
(require '99-keybindings)
;;; init.el ends here
