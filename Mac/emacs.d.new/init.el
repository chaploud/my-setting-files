;;; init.el --- メイン設定ファイル -*- lexical-binding: t; -*-

;;; Commentary:
;; パッケージ管理の初期化と各設定ファイルの読み込み

;;; Code:

;; パッケージ設定
(require 'package)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")
                         ("melpa" . "https://melpa.org/packages/")))
(package-initialize)

;; use-package (Emacs 29+で組み込み)
(require 'use-package)

;; lisp/をload-pathに追加
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; 設定ファイルを順番に読み込み
(require '00-core)
(require '01-ui)
(require '02-completion)
(require '03-evil)
(require '04-japanese)
(require '05-git)
(require '06-lsp)
(require '07-langs)
(require '08-tools)
(require '09-ai)
(require '99-keybinds)

;;; init.el ends here
