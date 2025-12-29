;;; 06-lsp.el --- LSP/Tree-sitter -*- lexical-binding: t; -*-

;;; Commentary:
;; eglot, tree-sitter, eldoc-box

;;; Code:

;; treesit - Tree-sitter設定
(use-package treesit
  :custom (treesit-font-lock-level 4)
  :config
  (setq treesit-language-source-alist
        '((clojure "https://github.com/sogaiu/tree-sitter-clojure")
          (dockerfile "https://github.com/camdencheek/tree-sitter-dockerfile")
          (hcl "https://github.com/tree-sitter-grammars/tree-sitter-hcl")
          (toml "https://github.com/ikatyang/tree-sitter-toml")
          (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
          (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
          (yaml "https://github.com/ikatyang/tree-sitter-yaml")))

  (defun my-treesit-install-grammars ()
    "未インストールのgrammarを自動インストール"
    (interactive)
    (dolist (lang '(bash c clojure cpp css dockerfile hcl html java javascript
                         json python ruby rust toml tsx typescript yaml))
      (unless (treesit-language-available-p lang)
        (condition-case err
            (treesit-install-language-grammar lang)
          (error (message "[treesit] FAILED %s: %s" lang err))))))

  (add-hook 'emacs-startup-hook
            (lambda () (run-with-idle-timer 1 nil #'my-treesit-install-grammars))))

;; eglot - LSPクライアント
(use-package eglot
  :custom
  (eglot-autoshutdown t)
  (eglot-connect-timeout 120)
  (eglot-events-buffer-config '(:size nil))
  (eldoc-echo-area-use-multiline-p nil)
  :bind (:map eglot-mode-map ("C-." . eglot-code-actions))
  :hook
  (bash-ts-mode . eglot-ensure)
  (c-ts-mode . eglot-ensure)
  (clojure-ts-mode . eglot-ensure)
  (css-ts-mode . eglot-ensure)
  (dockerfile-ts-mode . eglot-ensure)
  (html-ts-mode . eglot-ensure)
  (java-ts-mode . eglot-ensure)
  (js-ts-mode . eglot-ensure)
  (json-ts-mode . eglot-ensure)
  (python-ts-mode . eglot-ensure)
  (ruby-ts-mode . eglot-ensure)
  (rust-ts-mode . eglot-ensure)
  (terraform-mode . eglot-ensure)
  (tsx-ts-mode . eglot-ensure)
  (typescript-ts-mode . eglot-ensure)
  (yaml-ts-mode . eglot-ensure)
  (zig-mode . eglot-ensure))

;; eglot-tempel - LSPスニペット連携
(use-package eglot-tempel
  :ensure t
  :after eglot
  :config (eglot-tempel-mode t))

;; eldoc-box - eldocをポップアップ表示
(use-package eldoc-box :ensure t)

;; jarchive - jarファイル内ジャンプ (Java/Clojure)
(use-package jarchive
  :ensure t
  :after eglot
  :config (jarchive-setup))

(provide '06-lsp)
;;; 06-lsp.el ends here
