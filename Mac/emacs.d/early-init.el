;;; early-init.el --- Emacsの早期初期化設定 -*- lexical-binding: t; -*-
(setenv "LIBRARY_PATH"
        (string-join
         '("/opt/homebrew/opt/gcc/lib/gcc/15"
           "/opt/homebrew/opt/libgccjit/lib/gcc/15"
           "/opt/homebrew/opt/gcc/lib/gcc/current/gcc/aarch64-apple-darwin24/15")
         ":"))

;; === GCを抑制し、起動を高速化する
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

;; === 起動後に適切なGC設定に戻す
(add-hook 'emacs-startup-hook
	  (lambda ()
	    (setq gc-cons-percentage 0.3
		  gc-cons-threshold (* 256 1024 1024) ; 256MB
                  read-process-output-max (* 2 1024 1024) ; 2MB
		  garbage-collection-messages t)
	    (add-hook 'focus-out-hook #'garbage-collect)
            (run-with-idle-timer 5 t #'garbage-collect)))

;; GUIをスッキリさせる
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; スタートアップ画面を無効化
(setq inhibit-startup-screen t)

;; ファイル選択ウィンドウを表示しない
(setq use-file-dialog nil)
;; Xリソースを使用しない
(setq inhibit-x-resources t)
;; バッファメニューの使用を抑制
(setq inhibit-startup-buffer-menu t)

;; 画面最大化
(push '(fullscreen . maximized) default-frame-alist)

;; パッケージシステムを有効化
(setq package-enable-at-startup t)

;; ネイティブコンパイルの警告を抑制
(setq native-comp-async-report-warnings-errors 'silent)

;; ファイル名が対応する.elcファイルより新しい場合、.elファイルを優先的に読み込む
(setq load-prefer-newer t)

(provide 'early-init)
