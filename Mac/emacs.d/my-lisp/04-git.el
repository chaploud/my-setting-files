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

(defun my/diff-hl-sync ()
  "外部git変更時にdiff-hlを更新"
  (when buffer-file-name
    (ignore-errors (diff-hl-update))))

;; バッファ/ウィンドウ切り替え時
(add-hook 'window-buffer-change-functions (lambda (_) (my/diff-hl-sync)))
;; Emacsにフォーカスが戻った時
;; (add-hook 'focus-in-hook #'my/diff-hl-sync)

;; ediff設定
(use-package ediff
  :ensure nil
  :custom
  (ediff-window-setup-function 'ediff-setup-windows-plain)
  (ediff-split-window-function 'split-window-horizontally))

(provide '04-git)
;;; 04-git.el ends here
