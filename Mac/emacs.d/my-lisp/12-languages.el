;;; 12-languages.el --- 言語別設定 -*- lexical-binding: t; -*-

;;; Commentary:
;;; Code:

;; Markdown
(use-package markdown-mode
  :ensure t
  :mode ("\\.md\\'" . gfm-mode)
  :init
  (setq markdown-command '("pandoc" "--from=markdown" "--to=html5"))
  :bind
  (:map markdown-mode-map
        ("<backtab>" . markdown-promote)
        ("<normal-state> S-<tab>" . markdown-promote))
  :custom
  (markdown-fontify-code-blocks-natively t)
  (markdown-indent-on-enter 'indent-and-new-item)
  (markdown-gfm-use-electric-backquote nil))

;; Shell Script (sh/bash/zsh)
;; npm i -g bash-language-server
;; brew install shfmt
(use-package sh-script
  :ensure nil
  :mode (("\\.\\(sh\\|bash\\)\\'" . bash-ts-mode) ; sh/bash
         ("\\.?\\(bashrc\\|bash_profile\\)\\'" . bash-ts-mode) ; bash
         ("\\.?zsh\\(rc\\|env\\|profile\\)?\\'" . bash-ts-mode)) ; zsh
  :interpreter (("sh"   . bash-ts-mode)
                ("bash" . bash-ts-mode)
                ("zsh"  . bash-ts-mode))
  :custom
  (sh-basic-offset 2)
  (sh-indentation  2))

;; HTML/CSS/JSON
;; npm i -g vscode-langservers-extracted
(use-package html-ts-mode
  :ensure nil
  :mode "\\.x?html\\'")

(use-package css-ts-mode
  :ensure nil
  :mode "\\.css\\'")

(use-package json-ts-mode
  :ensure nil
  :mode (("\\.json\\'" . json-ts-mode)
         ("\\.arb\\'" . arb-mode))
  :config
  (define-derived-mode arb-mode json-ts-mode "ARB"
    "Major mode for editing ARB files."
    (setq-local tab-width 4)
    (setq-local standard-indent 4)
    (setq-local json-ts-mode-indent-offset 4)))

;; JavaScript / JSX / TypeScript / TSX
;; npm i -g typescript typescript-language-server
(use-package js-ts-mode
  :ensure nil
  :mode (("\\.js\\'" . js-ts-mode)
         ("\\.cjs\\'" . js-ts-mode)
         ("\\.mjs\\'" . js-ts-mode)
         ("\\.jsx\\'" . js-ts-mode))
  :config
  (js-indent-level 2))

(use-package typescript-ts-mode
  :ensure nil
  :mode "\\.ts\\'")

(use-package tsx-ts-mode
  :ensure nil
  :mode "\\.tsx\\'")

;; Docker
;; npm i -g dockerfile-language-server-nodejs
(use-package dockerfile-ts-mode
  :ensure nil
  :mode (("Dockerfile\\'" . dockerfile-ts-mode)
         ("\\.dockerfile\\'" . dockerfile-ts-mode)))


;; YAML
;; npm i -g yaml-language-server
(use-package yaml-ts-mode
  :ensure nil
  :mode ("\\.ya?ml\\'" . yaml-ts-mode))

;; Terraform (.tf)
;; brew install terraform-ls
(use-package terraform-mode
  :ensure t
  :mode "\\.tf\\'")

;; Groovy DSL (.gradle)
(use-package groovy-mode
  :ensure t
  :mode "\\.gradle\\'")

;; Python
;; pipx install 'python-lsp-server[all]'
(use-package python-ts-mode
  :ensure nil
  :mode "\\.py\\'"
  :interpreter ("python" . python-ts-mode))

;; Ruby
(use-package ruby-ts-mode
  :ensure nil
  :mode (("\\.rb\\'" . ruby-ts-mode)
         ("\\.rake\\'" . ruby-ts-mode)
         ("Rakefile\\'" . ruby-ts-mode)
         ("Gemfile\\'" . ruby-ts-mode)))

;; Rust
(use-package rust-ts-mode
  :ensure nil
  :mode "\\.rs\\'")

;; TOML
(use-package toml-ts-mode
  :ensure nil
  :mode "\\.toml\\'")

;; Java
;; brew install jdtls
(use-package java-ts-mode
  :ensure nil
  :mode "\\.java\\'")

;; C言語
;; brew install llvm
(use-package c-ts-mode
  :ensure nil
  :mode (("\\.c\\'" . c-ts-mode)
         ("\\.h\\'" . c-ts-mode)))

;; Zig
;; brew install zig
;; brew install zls
(use-package zig-mode
  :ensure t
  :mode (("\\.zig\\'" . zig-mode)
         ("\\.zon\\'" . zig-mode))
  :hook
  (eglot-managed-mode-hook
   .
   (lambda ()
     (when (eq major-mode 'zig-mode)
       (eglot-inlay-hints-mode -1))))
  :config
  (setq compilation-auto-jump-to-first-error t))

;; Makefile
;; brew install checkmake
(add-to-list 'auto-mode-alist '("\\(?:[Mm]akefile\\|\\.mk\\)\\'" . makefile-mode))
(add-hook 'makefile-mode-hook
          (lambda ()
            (setq-local indent-tabs-mode t)
            (setq-local tab-width 4)))
(add-hook 'compilation-filter-hook 'ansi-color-compilation-filter)

;; WebAssembly (wat/wast)
(use-package wat-ts-mode
  :ensure t
  :vc (:url "https://github.com/nverno/wat-ts-mode" :rev :newest :branch "master")
  :mode (("\\.wat\\'" . wat-ts-mode)
         ("\\.wast\\'" . wat-ts-mode)))

;; DB接続
;; go install github.com/sqls-server/sqls@latest
(use-package sql
  :ensure nil
  :custom
  (sql-postgres-login-params nil)
  (sql-connection-alist
   '(;; SQLアンチパターン
     (sql-antipatterns
      (sql-product 'mysql)
      (sql-user "root")
      (sql-password "")
      (sql-server "localhost")
      (sql-port 3306)
      (sql-database "anti_patterns"))))
  :hook (sql-mode . (lambda () (sql-indent-enable))))

(provide '12-languages)
;;; 12-languages.el ends here
