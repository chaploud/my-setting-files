;;; 02-completion.el --- 補完システム -*- lexical-binding: t; -*-

;;; Commentary:
;; ミニバッファ補完 (vertico) とバッファ内補完 (corfu)

;;; Code:

;; 再帰的ミニバッファを許可
(setq enable-recursive-minibuffers t)

;; vertico - 垂直補完UI
(use-package vertico
  :ensure t
  :custom
  (vertico-cycle t)
  (vertico-count 15)
  :config (vertico-mode t))

;; orderless - 柔軟なマッチング
(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

;; marginalia - 補完候補に注釈
(use-package marginalia
  :ensure t
  :config (marginalia-mode t))

;; consult - 便利な検索コマンド群
(use-package consult
  :ensure t
  :custom (consult-async-min-input 2)
  :init
  (setq xref-show-xrefs-function #'consult-xref)
  (setq xref-show-definitions-function #'consult-xref)
  :config
  (setq consult-ripgrep-args
        (concat consult-ripgrep-args
                " --hidden --glob=!.git/** --glob=!node_modules/** --glob=!target/**")))

;; embark - 補完候補へのアクション
(use-package embark
  :ensure t
  :bind (:map minibuffer-local-map
              ("C-." . embark-act)
              ("C-," . embark-export)))

(use-package embark-consult
  :ensure t
  :hook (embark-collect-mode . consult-preview-at-point-mode))

;; wgrep - grepバッファを編集可能に
(use-package wgrep
  :ensure t
  :custom
  (wgrep-auto-save-buffer t)
  (wgrep-change-readonly-file t))

;; corfu - バッファ内補完UI
(use-package corfu
  :ensure t
  :custom
  (corfu-auto t)
  (corfu-auto-delay 0.15)  ; 0は非推奨
  (corfu-auto-prefix 2)    ; 2文字から補完開始
  (corfu-cycle t)
  (corfu-preselect 'prompt)
  (corfu-quit-no-match 'separator)
  (tab-always-indent 'complete)
  :config
  (global-corfu-mode t)
  (corfu-popupinfo-mode t)
  (corfu-history-mode t))

;; nerd-icons-corfu
(use-package nerd-icons-corfu
  :ensure t
  :after corfu
  :config (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

;; cape - 補完ソース拡張
(use-package cape
  :ensure t
  :init
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-dabbrev))

;; tempel - スニペット
(use-package tempel
  :ensure t
  :init
  (add-hook 'completion-at-point-functions #'tempel-complete))

(provide '02-completion)
;;; 02-completion.el ends here
