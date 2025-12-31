;;; 07-lsp.el --- Tree-sitter/LSP -*- lexical-binding: t; -*-

;;; Commentary:
;;; Code:

;; 診断表示
(use-package flymake
  :ensure nil
  :custom
  (flymake-show-diagnostics-at-end-of-line t))

;; Tree-sitter設定
(use-package treesit
  :ensure nil
  :custom
  ;; === tree-sitterによる色付けmax
  (treesit-font-lock-level 4)
  :config
  ;; === tree-sitterの文法をインストールしたいリスト
  (defvar my-treesit-language-list
    '(bash
      c
      clojure
      cpp
      css
      dockerfile
      groovy
      hcl
      html
      java
      javascript
      jsdoc
      json
      python
      ruby
      rust
      toml
      tsx
      typescript
      wast
      wat
      yaml)
    "List of tree-sitter languages to install.")
  ;; === tsの参照URLを指定
  (setq treesit-language-source-alist
        '(
          (bash "https://github.com/tree-sitter/tree-sitter-bash")
          (c "https://github.com/tree-sitter/tree-sitter-c")
          (clojure "https://github.com/sogaiu/tree-sitter-clojure")
          (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
          (css "https://github.com/tree-sitter/tree-sitter-css")
          (dockerfile "https://github.com/camdencheek/tree-sitter-dockerfile")
          (groovy "https://github.com/murtaza64/tree-sitter-groovy")
          (hcl "https://github.com/tree-sitter-grammars/tree-sitter-hcl")
          (html "https://github.com/tree-sitter/tree-sitter-html")
          (java "https://github.com/tree-sitter/tree-sitter-java")
          (javascript "https://github.com/tree-sitter/tree-sitter-javascript")
          (jsdoc "https://github.com/tree-sitter/tree-sitter-jsdoc")
          (json "https://github.com/tree-sitter/tree-sitter-json")
          (python "https://github.com/tree-sitter/tree-sitter-python")
          (ruby "https://github.com/tree-sitter/tree-sitter-ruby")
          (rust "https://github.com/tree-sitter/tree-sitter-rust")
          (toml "https://github.com/ikatyang/tree-sitter-toml")
          (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
          (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
          (wast "https://github.com/wasm-lsp/tree-sitter-wasm.git" "main" "wast/src")
          (wat "https://github.com/wasm-lsp/tree-sitter-wasm.git" "main" "wat/src")
          (yaml "https://github.com/ikatyang/tree-sitter-yaml")
          ))
  ;; === 未導入だけ自動インストール
  (defun my-treesit-install-grammars ()
    "my-treesit-language-listのうち未導入のgrammarを自動インストールする"
    (interactive)
    (dolist (lang my-treesit-language-list)
      (unless (treesit-language-available-p lang)
        (condition-case err
            (progn
              (message "[treesit] Installing grammar for %s..." lang)
              (treesit-install-language-grammar lang)
              (message "[treesit] Installed: %s..." lang))
          (error
           (message "[treesit] FAILED %s → %s" lang (error-message-string err)))))))

  (add-hook 'emacs-startup-hook
            (lambda () (run-with-idle-timer 1 nil #'my-treesit-install-grammars)))
  )


;; eglot - LSPクライアント
;; [Clojure] brew install clojure-lsp/brew/clojure-lsp-native
;; [Bash] npm i -g bash-language-server
;; [C/C++] brew install llvm
;; [HTML/CSS/JSON] npm i -g vscode-langservers-extracted
;; [JavaScript/JSX/TypeScript/TSX] npm i -g typescript typescript-language-server
;; [Dockerfile] npm i -g dockerfile-language-server-nodejs
;; [YAML] npm i -g yaml-language-server
;; [Terraform] brew install terraform-ls
;; [Java] brew install jdtls
;; [Python] pipx install 'python-lsp-server[all]'
;; [Ruby] brew install solargraph
;; [Rust] rustupのインストール
;; [Zig] brew install zls
;; [SQL] go install github.com/sqls-server/sqls@latest
(use-package eglot
  :ensure nil
  :custom
  (eglot-events-buffer-config '(:size nil :format full))
  (eglot-autoshutdown t)
  (eglot-connect-timeout 360)
  (eglot-extend-to-xref nil)
  (eldoc-echo-area-use-multiline-p nil)

  :bind
  (:map eglot-mode-map
        ("C-." . eglot-code-actions))

  :hook
  ;; LSP自動起動したい場合はここに追加
  (bash-ts-mode . eglot-ensure)
  (clojure-ts-mode . eglot-ensure)
  (dockerfile-ts-mode . eglot-ensure)
  (json-ts-mode . eglot-ensure)
  (python-ts-mode . eglot-ensure)
  (ruby-ts-mode . eglot-ensure)
  (rust-ts-mode . eglot-ensure)
  (sql-mode . eglot-ensure)
  (terraform-mode . eglot-ensure)
  (yaml-ts-mode . eglot-ensure)
  (zig-mode . eglot-ensure)

  :config
  (defun my-eglot-start ()
    "Start eglot for the current buffer if not already started."
    (interactive)
    (eglot-ensure))

  ;; デフォルトと変えたいものは指定
  (defvar my-eglot-server-list
    '((sql-mode . ("sqls")))
    "Custom eglot server programs.")

  (dolist (pair my-eglot-server-list)
    (add-to-list 'eglot-server-programs pair)))

;; 自動フォーマット
(use-package format-all
  :ensure t)

;; 言語別フォーマット設定
;; lsp: eglot-format-buffer を優先
;; all: format-all-buffer を使用
(defvar my-format-config
  '((clojure-ts-mode    lsp all)
    (json-ts-mode       lsp all)
    (zig-mode           lsp all)
    (emacs-lisp-mode    all))
  "Format configuration per mode. lsp = eglot, all = format-all.")

(defun my-format-buffer ()
  "Format buffer based on `my-format-config'."
  (interactive)
  (when-let ((config (alist-get major-mode my-format-config)))
    (let ((use-lsp (and (memq 'lsp config)
                        (bound-and-true-p eglot-managed-mode)))
          (use-all (memq 'all config)))
      (cond
       (use-lsp (call-interactively #'eglot-format))
       (use-all (call-interactively #'format-all-buffer))))))

(add-hook 'before-save-hook #'my-format-buffer)

(provide '07-lsp)
;;; 07-lsp.el ends here
