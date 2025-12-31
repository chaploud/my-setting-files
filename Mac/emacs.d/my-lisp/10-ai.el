;;; 10-ai.el --- AI連携 -*- lexical-binding: t; -*-

;;; Commentary:
;; 初回セットアップ:
;; - M-x copilot-install-server (GitHub Copilot)
;; - Claude Code CLIのインストールとAPIキー設定

;;; Code:

;; GitHub Copilot
(use-package copilot
  :ensure t
  :vc (:url "https://github.com/copilot-emacs/copilot.el" :rev :newest :branch "main")
  :hook (prog-mode . copilot-mode)
  :bind
  (:map copilot-completion-map
        ("C-<tab>" . copilot-accept-completion))
  (:map prog-mode-map
        ("M-/" . copilot-complete))
  :custom (copilot-max-char 1000000)    ; 最大文字数を増やす
  :config
  (add-to-list 'copilot-indentation-alist '(prog-mode  2))
  (add-to-list 'copilot-indentation-alist '(org-mode  2))
  (add-to-list 'copilot-indentation-alist '(text-mode  2))
  (add-to-list 'copilot-indentation-alist '(emacs-lisp-mode  2)))

;; Claude Code IDE
;; https://github.com/manzaltu/claude-code-ide.el
(use-package claude-code-ide
  :ensure t
  :vc (:url "https://github.com/manzaltu/claude-code-ide.el" :rev :newest)
  :commands (claude-code-ide claude-code-ide-menu)
  :custom
  (claude-code-ide-window-width 0.4)
  (claude-code-ide-use-ide-diff nil)    ; ediff機能を無効化
  :config
  (claude-code-ide-emacs-tools-setup)

  ;; 崩れないように*claude-code[...]*バッファのフォントを変更
  (defun my-set-font-for-claude-buffer ()
    "Set a specific font for Claude Code IDE buffers."
    (when (string-match-p "^\\*claude-code" (buffer-name))
      (buffer-face-set :family "UDEV Gothic 35NF" :height 130)))
  (add-hook 'buffer-list-update-hook #'my-set-font-for-claude-buffer)

  ;; vterm-mode-map へのバインディングは vterm がロード後に設定
  (with-eval-after-load 'vterm
    (define-key vterm-mode-map (kbd "C-c C-e") #'claude-code-ide-send-escape))

  ;; テキストを Claude Code に送信（バッファを特定して送る）
  (defun my-claude-send-text (text)
    "Send TEXT to Claude Code."
    (let* ((project-dir (claude-code-ide--get-working-directory))
           (claude-buffer-name (claude-code-ide--get-buffer-name project-dir))
           (claude-buffer (get-buffer claude-buffer-name)))
      (unless claude-buffer
        (user-error "Claude Code IDEが起動していません"))
      (with-current-buffer claude-buffer
        (claude-code-ide--terminal-send-string text)
        (sit-for 0.1)
        (claude-code-ide--terminal-send-return))
      (message "Claude に送信: %s..." (substring text 0 (min 50 (length text))))))

  ;; 選択範囲を送信
  (defun my-claude-send-region ()
    "Send region to Claude Code."
    (interactive)
    (if (region-active-p)
        (let ((text (buffer-substring-no-properties (region-beginning) (region-end))))
          (when (and (bound-and-true-p evil-mode) (evil-visual-state-p))
            (evil-exit-visual-state))
          (my-claude-send-text text))
      (user-error "リージョンが選択されていません")))

  ;; 数字送信
  (defun my-claude-send-number (n)
    "Send number N to Claude Code."
    (my-claude-send-text (number-to-string n)))

  (defun my-claude-send-1 () "Send 1." (interactive) (my-claude-send-number 1))
  (defun my-claude-send-2 () "Send 2." (interactive) (my-claude-send-number 2))
  (defun my-claude-send-3 () "Send 3." (interactive) (my-claude-send-number 3))
  )

;; スクラッチバッファ
(defun my-claude-scratch ()
  "Toggle Claude Code scratch buffer for current project."
  (interactive)
  (let* ((project-dir (claude-code-ide--get-working-directory))
         (project-name (file-name-nondirectory (directory-file-name project-dir)))
         (buffer-name (format "*claude-scratch[%s]*" project-name))
         (buffer (get-buffer buffer-name))
         (window (and buffer (get-buffer-window buffer))))
    (cond
     ;; 表示中なら閉じる
     (window (delete-window window))
     ;; バッファがあれば表示
     (buffer (my-claude-scratch-show buffer))
     ;; なければ作成して表示
     (t
      (let ((new-buffer (get-buffer-create buffer-name)))
        (with-current-buffer new-buffer
          (insert (format "Claude Code scratch [%s]\n\n" project-name))
          (setq-local truncate-lines nil))
        (my-claude-scratch-show new-buffer))))))

(defun my-claude-scratch-show (buffer)
  "Show scratch BUFFER below the leftmost window."
  (let* ((base (frame-first-window))
         (win (split-window base -15 'below)))
    (set-window-buffer win buffer)
    (set-window-dedicated-p win t)
    (select-window win)
    (goto-char (point-max))))

(defun my-claude-code-ide ()
  "Toggle Claude Code IDE and scratch buffer together."
  (interactive)
  (call-interactively #'claude-code-ide)
  (call-interactively #'my-claude-scratch))

(defun my-claude-code-ide-resume ()
  "Resume Claude Code IDE with scratch buffer."
  (interactive)
  (call-interactively #'claude-code-ide-resume)
  (call-interactively #'my-claude-scratch))

(provide '10-ai)
;;; 10-ai.el ends here
