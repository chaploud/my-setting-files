;;; early-init.el --- Emacsの早期初期化設定 -*- lexical-binding: t; -*-

;; === ネイティブコンパイルをMacで動作させるためパスを通す
(setenv "LIBRARY_PATH"
        (string-join
         '("/opt/homebrew/opt/gcc/lib/gcc/15"
           "/opt/homebrew/opt/libgccjit/lib/gcc/15"
           "/opt/homebrew/opt/gcc/lib/gcc/current/gcc/aarch64-apple-darwin24/15")
         ":"))

;; === GCを抑制し、起動を高速化する
(setq gc-cons-threshold (* 1024 1024 1024) ; 1GB
      gc-cons-percentage 0.6)

;; === 起動後に適切なGC設定に戻す
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-percentage 0.3
                  gc-cons-threshold (* 256 1024 1024)     ; 256MB
                  read-process-output-max (* 2 1024 1024) ; 2MB
                  )
            (add-hook 'focus-out-hook #'garbage-collect)
            (run-with-idle-timer 30 t #'garbage-collect)
            ;; 読み込み後、常に画面を最大化する
            (toggle-frame-maximized)
            )
          )

;; === GUIをスッキリさせる
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; === ネイティブコンパイルの警告を抑制する
(setq native-comp-async-report-warnings-errors 'silent)
