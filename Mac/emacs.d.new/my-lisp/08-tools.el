;;; 08-tools.el --- ツール群 -*- lexical-binding: t; -*-

;;; Commentary:
;; vterm, treemacs, perspective, elfeed, restclient, bookmark, bufferfile

;;; Code:

;; vterm
(use-package vterm
  :ensure t
  :custom
  (vterm-max-scrollback 100000)
  :hook (vterm-mode . (lambda ()
                        (setq-local nobreak-char-display nil)
                        (evil-insert-state)))
  :config
  ;; プロジェクト単位でvtermをトグル
  (defun my-vterm-toggle ()
    "Toggle project-specific vterm."
    (interactive)
    (let* ((proj (project-current))
           (proj-name (if proj
                          (file-name-nondirectory (directory-file-name (project-root proj)))
                        "default"))
           (buf-name (format "*vterm[%s]*" proj-name))
           (buf (get-buffer buf-name))
           (win (and buf (get-buffer-window buf))))
      (cond
       ((and win (eq (selected-window) win)) (quit-window))
       (win (select-window win))
       (buf (pop-to-buffer buf))
       (t (let ((default-directory (if proj (project-root proj) default-directory)))
            (vterm buf-name))))))

  ;; 新規vterm作成
  (defun my-vterm-new ()
    "Create new vterm buffer."
    (interactive)
    (let ((default-directory (if (project-current)
                                 (project-root (project-current))
                               default-directory)))
      (vterm (generate-new-buffer-name "*vterm*")))))

;; treemacs
(use-package treemacs
  :ensure t
  :defer t
  :custom
  (treemacs-follow-mode t)
  (treemacs-project-follow-cleanup t))

(use-package treemacs-evil
  :ensure t
  :after (treemacs evil))

(use-package treemacs-nerd-icons
  :ensure t
  :after treemacs
  :config (treemacs-nerd-icons-config))

;; perspective - ワークスペース
(use-package perspective
  :ensure t
  :init (setq persp-suppress-no-prefix-key-warning t)
  :custom
  (persp-sort 'created)
  (persp-modestring-short t)
  :config (persp-mode))

(use-package treemacs-perspective
  :ensure t
  :after (treemacs perspective)
  :config (treemacs-set-scope-type 'Perspectives))

;; elfeed - RSSリーダー
(use-package elfeed
  :ensure t
  :custom
  (elfeed-search-filter "@1-week-ago +unread")
  (shr-use-fonts nil))

;; restclient
(use-package restclient
  :ensure t
  :mode (("\\.http\\'" . restclient-mode)
         ("\\.rest\\'" . restclient-mode))
  :custom (restclient-same-buffer-response t))

;; bookmark
(use-package bookmark
  :custom (bookmark-save-flag 1))

;; bufferfile - ファイルのリネーム・削除
(use-package bufferfile :ensure t)

;; docker
(use-package docker
  :ensure t
  :custom (docker-container-default-sort-key '("Names")))

(provide '08-tools)
;;; 08-tools.el ends here
