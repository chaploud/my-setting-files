;;; 07-langs.el --- 言語別設定 -*- lexical-binding: t; -*-

;;; Commentary:
;; 各プログラミング言語のモード設定

;;; Code:

;; Clojure
(use-package clojure-ts-mode
  :ensure t
  :custom (clojure-ts-toplevel-inside-comment-form t))

(use-package cider
  :ensure t
  :hook (clojure-ts-mode . cider-mode)
  :custom
  (cider-repl-buffer-size-limit 10000)
  (cider-repl-pop-to-buffer-on-connect nil)
  (cider-use-xref nil))

;; puni - 構造的編集 (Lisp系)
(use-package puni
  :ensure t
  :hook
  (emacs-lisp-mode . puni-mode)
  (clojure-ts-mode . puni-mode))

;; Shell
(use-package sh-script
  :mode (("\\.sh\\'" . bash-ts-mode)
         ("\\.bash\\'" . bash-ts-mode)
         ("\\.zsh\\(rc\\|env\\)?\\'" . bash-ts-mode))
  :custom (sh-basic-offset 2))

;; C
(use-package c-ts-mode
  :mode (("\\.c\\'" . c-ts-mode)
         ("\\.h\\'" . c-ts-mode)))

;; HTML/CSS/JSON
(use-package html-ts-mode :mode "\\.html\\'")
(use-package css-ts-mode :mode "\\.css\\'")
(use-package json-ts-mode :mode "\\.json\\'")

;; JavaScript/TypeScript
(use-package js-ts-mode
  :mode (("\\.js\\'" . js-ts-mode)
         ("\\.jsx\\'" . js-ts-mode)))
(use-package typescript-ts-mode :mode "\\.ts\\'")
(use-package tsx-ts-mode :mode "\\.tsx\\'")

;; Docker
(use-package dockerfile-ts-mode
  :mode ("Dockerfile\\'" "\\.dockerfile\\'"))

;; YAML
(use-package yaml-ts-mode :mode "\\.ya?ml\\'")

;; Terraform
(use-package terraform-mode
  :ensure t
  :mode "\\.tf\\'")

;; Java
(use-package java-ts-mode :mode "\\.java\\'")

;; Python
(use-package python-ts-mode :mode "\\.py\\'")

;; Ruby
(use-package ruby-ts-mode
  :mode (("\\.rb\\'" . ruby-ts-mode)
         ("Gemfile\\'" . ruby-ts-mode)))

;; Rust
(use-package rust-ts-mode :mode "\\.rs\\'")

;; TOML
(use-package toml-ts-mode :mode "\\.toml\\'")

;; Zig
(use-package zig-mode
  :ensure t
  :mode "\\.zig\\'"
  :hook (eglot-managed-mode . (lambda ()
                                (when (eq major-mode 'zig-mode)
                                  (eglot-inlay-hints-mode -1)))))

;; Makefile
(add-to-list 'auto-mode-alist '("Makefile\\'" . makefile-mode))
(add-hook 'makefile-mode-hook (lambda () (setq-local indent-tabs-mode t)))
(add-hook 'compilation-filter-hook 'ansi-color-compilation-filter)

;; Markdown
(use-package markdown-mode
  :ensure t
  :mode ("\\.md\\'" . gfm-mode)
  :custom
  (markdown-fontify-code-blocks-natively t)
  (markdown-gfm-use-electric-backquote nil))

(provide '07-langs)
;;; 07-langs.el ends here
