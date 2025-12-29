;;; 04-japanese.el --- 日本語入力 -*- lexical-binding: t; -*-

;;; Commentary:
;; ddskk + yaskkserv2 + macSKK連携

;;; Code:

;; macism - Emacs外とのIME切り替え連携
(defun my-switch-ime (input-source)
  "Switch to INPUT-SOURCE (requires macism)."
  (call-process "macism" nil 0 nil input-source))

(add-function :after after-focus-change-function
              (lambda ()
                (when (frame-focus-state)
                  (my-switch-ime "net.mtgto.inputmethod.macSKK.ascii"))))

;; ccc - カーソル色制御
(use-package ccc :ensure t)

;; ddskk
(use-package ddskk
  :ensure t
  :custom
  (skk-server-host "127.0.0.1")
  (skk-server-portnum 1178)
  (skk-dcomp-activate t)
  (skk-egg-like-newline t)
  (skk-delete-implies-kakutei nil)
  (skk-show-candidates-nth-henkan-char 3)
  (skk-isearch-mode-enable 'always)
  (skk-use-color-cursor t)
  ;; カーソル色 (catppuccin-macchiato)
  (skk-cursor-default-color "#cad3f5")
  (skk-cursor-latin-color "#cad3f5")
  (skk-cursor-hiragana-color "#f5bde6")
  (skk-cursor-katakana-color "#a6da95")
  (skk-cursor-abbrev-color "#c6a0f6")
  :hook
  (isearch-mode . skk-isearch-mode-setup)
  (isearch-mode . skk-latin-mode-on)
  (isearch-mode-end . skk-isearch-mode-cleanup)
  (evil-normal-state-entry . skk-latin-mode-on)
  (text-mode . my-skk-latin)
  (prog-mode . my-skk-latin)
  :bind
  ("C-x j" . skk-mode)
  ("C-j" . skk-kakutei)
  :config
  (defun my-skk-latin ()
    "skk-modeを有効にして英字モードに"
    (skk-mode t)
    (skk-latin-mode-on)))

(provide '04-japanese)
;;; 04-japanese.el ends here
