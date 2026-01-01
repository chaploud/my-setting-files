;;; early-init.el --- 起動前の設定 -*- lexical-binding: t; -*-

;;; Commentary:
;; Emacs起動の最初期に実行される設定ファイル

;;; Code:

;; ネイティブコンパイル (Homebrew Apple Silicon)
(setenv "LIBRARY_PATH"
        (string-join
         '("/opt/homebrew/opt/gcc/lib/gcc/15"
           "/opt/homebrew/opt/libgccjit/lib/gcc/15"
           "/opt/homebrew/opt/gcc/lib/gcc/current/gcc/aarch64-apple-darwin24/15")
         ":"))
(setq native-comp-async-report-warnings-errors 'silent)

;; GC抑制（起動高速化）
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

;; 起動完了後に復元
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-percentage 0.3
                  gc-cons-threshold (* 256 1024 1024)     ; 256MB
                  read-process-output-max (* 4 1024 1024) ; 4MB
                  )
            (add-hook 'focus-out-hook #'garbage-collect)
            (run-with-idle-timer 30 t #'garbage-collect)
            (toggle-frame-maximized) ; 起動後に最大化
            ))

;; 読み込み完了までの見た目の摩擦を減らす
(add-to-list 'default-frame-alist '(background-color . "#24273a"))
(add-to-list 'default-frame-alist '(foreground-color . "#cad3f5"))
(custom-set-faces
 '(mode-line ((t (:background "#1e2030")))))

;; フレームタイトル
(setq frame-title-format "Emacs")
(setq ns-use-proxy-icon nil)

;; GUIをスッキリさせる
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;;; early-init.el ends here
