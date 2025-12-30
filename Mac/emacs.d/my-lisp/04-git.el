;;; 04-git.el --- Git連携 -*- lexical-binding: t; -*-

;;; Commentary:
;;; Code:

;; Git操作
(use-package magit
  :ensure t
  :custom
  (magit-blame-echo-style 'headings)
  :config
  (defun my-magit-blame-echo-toggle ()
    "Toggle `magit-blame-echo'."
    (interactive)
    (if (bound-and-true-p magit-blame-mode)
        (magit-blame-quit)
      (call-interactively 'magit-blame-echo))))

;; GitHub連携
;; GitHubのアクセストークンを取得しておくこと
;; ~/.authinfo.gpgにGitHubのアクセストークンを設定しておくこと
(use-package forge
  :ensure t
  :after magit
  :custom
  (forge-owned-accounts '("chaploud")))

;; フリンジに差分表示
(use-package diff-hl
  :ensure t
  :custom
  (global-diff-hl-mode t)
  (diff-hl-flydiff-mode t)
  :hook
  (magit-pre-refresh-hook  . diff-hl-magit-pre-refresh)
  (magit-post-refresh-hook . diff-hl-magit-post-refresh))

;; ediff設定
(use-package ediff
  :ensure nil
  :custom
  (ediff-window-setup-function 'ediff-setup-windows-plain)
  (ediff-split-window-function 'split-window-horizontally))

(provide '04-git)
;;; 04-git.el ends here
