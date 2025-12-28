;;; early-init.el --- 起動前の設定 -*- lexical-binding: t; -*-

;;; Commentary:
;; Emacs起動の最初期に実行される設定ファイル

;;; Code:

;; GC抑制（起動高速化）
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

;; file-name-handler-alist一時無効化
(defvar my--file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

;; 起動完了後に復元
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 64 1024 1024)
                  gc-cons-percentage 0.2
                  read-process-output-max (* 4 1024 1024))
            (setq file-name-handler-alist my--file-name-handler-alist)
            (add-hook 'focus-out-hook #'garbage-collect)
            (toggle-frame-maximized)))

;; ネイティブコンパイル (Homebrew Apple Silicon)
(setenv "LIBRARY_PATH"
        (string-join
         '("/opt/homebrew/opt/gcc/lib/gcc/15"
           "/opt/homebrew/opt/libgccjit/lib/gcc/15"
           "/opt/homebrew/opt/gcc/lib/gcc/current/gcc/aarch64-apple-darwin24/15")
         ":"))
(setq native-comp-async-report-warnings-errors 'silent)

;; GUI初期設定（ちらつき防止）
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(push '(background-color . "#24273a") default-frame-alist)
(push '(foreground-color . "#cad3f5") default-frame-alist)
(setq frame-title-format "Emacs")
(setq ns-use-proxy-icon nil)
(setq frame-inhibit-implied-resize t)

;; パッケージ初期化をinit.elで行う
(setq package-enable-at-startup nil)

;;; early-init.el ends here
