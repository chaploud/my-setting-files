;;; 04-git.el --- Git連携 -*- lexical-binding: t; -*-

;;; Commentary:
;;; Code:

;; Git操作
(use-package magit
  :ensure t
  :custom
  (magit-blame-echo-style 'headings)
  :config
  (defun my/magit-blame-echo-toggle ()
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
  :defer t
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

(defvar my/git-status-last nil)

;; 外部からのgit status変更を同期
(defun my/git-status-check ()
  (when-let ((root (vc-root-dir)))
    (let* ((default-directory root)
           (current (string-trim (shell-command-to-string "git rev-parse HEAD"))))
      (unless (equal current my/git-status-last)
        (setq my/git-status-last current)
        (diff-hl-update)
        (when (bound-and-true-p magit-status-mode)
          (magit-refresh))))
    ))

(run-with-timer 0 10 #'my/git-status-check) ; パフォーマンスに注意

;; ediff設定
(use-package ediff
  :ensure nil
  :custom
  (ediff-window-setup-function 'ediff-setup-windows-plain)
  (ediff-split-window-function 'split-window-horizontally))

(provide '04-git)
;;; 04-git.el ends here
