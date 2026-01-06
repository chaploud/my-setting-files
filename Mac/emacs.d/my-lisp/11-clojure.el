;;; 11-clojure.el --- Clojure開発環境 -*- lexical-binding: t; -*-

;;; Commentary:
;;; Code:

(use-package clojure-ts-mode
  :ensure t
  :custom
  (clojure-ts-toplevel-inside-comment-form t)
  :config
  ;; === clojure-lsp用キャッシュの削除 & 再起動
  (defun my-clojure-lsp-clear-cache-and-restart ()
    "Clear clojure-lsp cache and restart the server."
    (interactive)
    (call-interactively #'eglot-shutdown)
    (let* ((root-dir (project-root (project-current)))
           (lsp-cache (file-name-concat root-dir ".lsp/.cache"))
           (kondo-cache (file-name-concat root-dir ".clj-kondo/.cache")))
      (when (file-directory-p lsp-cache)
        (delete-directory lsp-cache t)
        (message "Deleted clojure-lsp cache at %s"  lsp-cache))
      (when (file-directory-p kondo-cache)
        (delete-directory kondo-cache t)
        (message "Deleted clj-kondo cache at %s"  kondo-cache)))
    (eglot-ensure)))

;; REPL連携
(use-package cider
  :ensure t
  :hook (clojure-ts-mode . cider-mode)
  :custom
  (cider-repl-buffer-size-limit 10000)
  (cider-font-lock-dynamically '(macro core function var deprecated))
  (cider-repl-pop-to-buffer-on-connect nil)
  (cider-use-xref nil)

  :config
  (defun my-cider-send-reload ()
    "Send (reload) to REPL"
    (interactive)
    (when (cider-connected-p)
      (cider-interactive-eval "(user/reload)"))))

;; S式構造的編集
(use-package puni
  :ensure t
  :demand t
  :hook
  ((emacs-lisp-mode . puni-mode)
   (lisp-interaction-mode . puni-mode)
   (clojure-ts-mode . puni-mode))
  :config
  ;; === puniでカーソル以降をそのレベルで閉じるまで削除
  (defun my-puni-kill-to-end ()
    "Kill to the end of the current sexp."
    (interactive)
    (let ((begin (point))
          (end (save-excursion (puni-end-of-sexp) (point))))
      (kill-region begin end)))

  ;; === puniでカーソル以降をそのレベルで閉じる直前までコピー
  (defun my-puni-yank-to-end ()
    "Kill to the end of the current sexp."
    (interactive)
    (let ((begin (point))
          (end (save-excursion (puni-end-of-sexp) (point))))
      (evil-yank begin end)
      (evil-goggles--show-blocking-hint begin end)))

  ;; === puniでシンボルを""で囲む
  (defun my-wrap-symbol-with-quotes ()
    "Surround the current symbol with double quotes."
    (interactive)
    (let ((bounds (bounds-of-thing-at-point 'symbol)))
      (when bounds
        (save-excursion
          (goto-char (cdr bounds))
          (insert "\"")
          (goto-char (car bounds))
          (insert "\""))))))

;; jarファイル内ジャンプ (Java/Clojure)
(use-package jarchive
  :ensure t
  :after eglot
  :config
  (jarchive-setup))

(provide '11-clojure)
;;; 11-clojure.el ends here
