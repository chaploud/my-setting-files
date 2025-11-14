;;; ~/.emacs.d/early-init.el --- EmacsのGUI起動前に実行される設定ファイル -*- lexical-binding: t; -*-

;;; Commentary:

;; 最終更新: 2025-11-13

;; === 前提
;; macOS Tahoe 26.1
;; gcc@15.2.0 インストール済み (https://formulae.brew.sh/formula/gcc)
;; libgccjit@15.2.0 インストール済み (https://formulae.brew.sh/formula/libgccjit)
;; emacs-plus@30.2 で利用 (https://github.com/d12frosted/homebrew-emacs-plus)
;; emacs 30.2

;;====================================================================
;; 起動時間最適化
;;===================================================================
;; file-name-handlerを一時的に無効化して起動時間を短縮
(defvar my--file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq file-name-handler-alist my--file-name-handler-alist)
            (makunbound 'my--file-name-handler-alist)))

;;====================================================================
;; 最初期のGUI設定
;;===================================================================

;; === 起動直後とinit.el読み込み完了までの見た目の摩擦を減らす
(add-to-list 'default-frame-alist '(background-color . "#24273a"))
(add-to-list 'default-frame-alist '(foreground-color . "#cad3f5"))
(custom-set-faces
 '(mode-line ((t (:background "#1e2030")))))

;; === フレームタイトル
(setq frame-title-format "Emacs")
(setq ns-use-proxy-icon nil)

;; === GUIをスッキリさせる
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;;====================================================================
;; ネイティブコンパイル・GC設定
;;===================================================================

;; === ネイティブコンパイルをMacで動作させるためパスを通す(環境に合わせ変更)
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
                  gc-cons-threshold (* 256 1024 1024)     ; 256MB
                  read-process-output-max (* 4 1024 1024) ; 4MB
                  )
            (add-hook 'focus-out-hook #'garbage-collect)
            (run-with-idle-timer 30 t #'garbage-collect)
            ;; 読み込み後、常に画面を最大化する
            (toggle-frame-maximized)))

;; === ネイティブコンパイルの警告を抑制する
(setq native-comp-async-report-warnings-errors 'silent)

;;; early-init.el ends here
