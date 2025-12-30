;;; 09-completion.el --- 補完システム -*- lexical-binding: t; -*-

;;; Commentary:
;;; Code:

;; バッファ内補完UI
(use-package corfu
  :ensure t
  :custom
  (global-corfu-mode t)
  (corfu-popupinfo-mode t)
  (corfu-history-mode t)
  (corfu-auto t)
  (corfu-auto-delay 0)
  (corfu-popupinfo-delay 0)
  (corfu-auto-prefix 1)
  (corfu-cycle t)
  (corfu-preselect 'prompt)
  (corfu-quit-no-match 'separator)
  (corfu-on-exact-match nil)
  (tab-always-indent 'complete))

;; 補完ポップアップのアイコン
(use-package nerd-icons-corfu
  :ensure t
  :after (corfu nerd-icons)
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

;; スニペット・テンプレート
(use-package tempel
  :ensure t)

;; LSPスニペット連携
(use-package eglot-tempel
  :ensure t
  :after eglot
  :config (eglot-tempel-mode t))

;; 補完ソースの統合・拡張 (cape)
(use-package cape
  :ensure t
  :hook
  (prog-mode . my-prog-capf)
  (text-mode . my-text-capf)
  :config
  (defun my-prog-capf ()
    (unless (local-variable-p 'my-prog-capf-configured)
      (if (bound-and-true-p eglot--managed-mode)
          (setq-local completion-at-point-functions
                      (cons (cape-capf-super #'eglot-completion-at-point
                                             #'tempel-expand
                                             #'cape-file)
                            completion-at-point-functions))
        (setq-local completion-at-point-functions
                    (cons (cape-capf-super #'tempel-complete
                                           #'cape-file)
                          completion-at-point-functions))))
    (setq-local my-prog-capf-configured t))

  (defun my-text-capf ()
    (unless (local-variable-p 'my-text-capf-configured)
      (setq-local completion-at-point-functions
                  (cons (cape-capf-super #'tempel-complete
                                         #'cape-dabbrev
                                         #'cape-file)
                        completion-at-point-functions)))
    (setq-local my-text-capf-configured t)))

(provide '09-completion)
;;; 09-completion.el ends here
