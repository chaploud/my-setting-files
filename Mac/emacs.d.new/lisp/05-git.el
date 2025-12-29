;;; 05-git.el --- Git連携 -*- lexical-binding: t; -*-

;;; Commentary:
;; magit, forge, diff-hl

;;; Code:

;; magit
(use-package magit
  :ensure t
  :custom (magit-blame-echo-style 'headings)
  :config
  (defun my-magit-blame-toggle ()
    "Toggle magit-blame-echo."
    (interactive)
    (if (bound-and-true-p magit-blame-mode)
        (magit-blame-quit)
      (call-interactively 'magit-blame-echo))))

;; forge - GitHub連携 (要: ~/.authinfo.gpg)
(use-package forge
  :ensure t
  :after magit
  :config (setq forge-owned-accounts '("chaploud")))

;; diff-hl - フリンジに差分表示
(use-package diff-hl
  :ensure t
  :hook
  (magit-pre-refresh . diff-hl-magit-pre-refresh)
  (magit-post-refresh . diff-hl-magit-post-refresh)
  :config
  (global-diff-hl-mode t)
  (diff-hl-flydiff-mode t))

;; ediff設定
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq ediff-split-window-function 'split-window-horizontally)

(provide '05-git)
;;; 05-git.el ends here
