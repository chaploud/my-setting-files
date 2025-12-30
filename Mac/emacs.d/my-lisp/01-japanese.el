;;; 01-japanese.el --- 日本語入力 -*- lexical-binding: t; -*-

;;; Commentary:
;;; Code:

(defun my-switch-ime (input-source)
  "Switch to INPUT-SOURCE (requires macism)."
  (call-process "macism" nil 0 nil input-source))

;; Emacsフォーカス時にIMEを英字モードに切り替え
(add-function
 :after after-focus-change-function
 (lambda ()
   (when (frame-focus-state)
     (my-switch-ime "net.mtgto.inputmethod.macSKK.ascii"))))

;; DDSKK(日本語入力)
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
  (skk-cursor-default-color "#cad3f5")
  (skk-cursor-latin-color "#cad3f5")
  (skk-cursor-hiragana-color "#f5bde6")
  (skk-cursor-katakana-color "#a6da95")
  (skk-cursor-abbrev-color "#c6a0f6")
  (skk-isearch-mode-string-alist ; isearchで[あ]などを非表示
   '((hiragana . "")
     (katakana . "")
     (jisx0208-latin . "")
     (latin . "")
     (abbrev . "")
     (nil . "")))
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

(provide '01-japanese)
;;; 01-japanese.el ends here
