;;; 10-ai.el --- AI連携 -*- lexical-binding: t; -*-

;;; Commentary:
;;; Code:

;; copilot (初回: M-x copilot-install-server)
(use-package copilot
  :ensure t
  :vc (:url "https://github.com/copilot-emacs/copilot.el" :rev :newest :branch "main")
  :hook
  (prog-mode . copilot-mode)
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

(use-package claude-code-ide
  :ensure t
  :commands (my-claude-code-ide-with-scratch)
  :vc (:url "https://github.com/manzaltu/claude-code-ide.el" :rev :newest)
  :custom
  (claude-code-ide-window-width 0.4)
  (claude-code-ide-focus-claude-after-ediff nil)
  :config
  (claude-code-ide-emacs-tools-setup)

  ;; 崩れないように*claude-code[...]*バッファのフォントを変更
  (defun my-set-font-for-claude-buffer ()
    "Set a specific font for Claude Code IDE buffers."
    (when (string-match-p "^\\*claude-code" (buffer-name))
      (buffer-face-set :family "UDEV Gothic 35NF" :height 130)))
  (add-hook 'buffer-list-update-hook #'my-set-font-for-claude-buffer)

  ;; スクラッチバッファのトグル表示
  (defun my-claude-code-ide-scratch ()
    "Toggle Claude Code scratch buffer."
    (interactive)
    (let* ((project-dir (claude-code-ide--get-working-directory))
           (buffer-name (format "*claude-scratch[%s]*"
                                (file-name-nondirectory (directory-file-name project-dir))))
           (scratch-buffer (get-buffer buffer-name))
           (scratch-window (and scratch-buffer (get-buffer-window scratch-buffer))))

      (cond
       ;; ウィンドウが表示されている場合は閉じる
       (scratch-window
        (delete-window scratch-window))

       ;; バッファは存在するが非表示の場合は表示する
       (scratch-buffer
        (my-claude-code-ide-scratch-show scratch-buffer project-dir))

       ;; バッファが存在しない場合は作成して表示
       (t
        (let ((claude-buffer (get-buffer (claude-code-ide--get-buffer-name project-dir))))
          (unless claude-buffer
            (user-error "Claude Code IDEが起動していません"))
          (let ((new-buffer (get-buffer-create buffer-name)))
            (with-current-buffer new-buffer
              (insert "*Claude Code IDE scratch*\n\n")
              (setq-local claude-scratch-project-dir project-dir)
              (setq-local truncate-lines nil))  ; 折り返しを有効化
            (my-claude-code-ide-scratch-show new-buffer project-dir)))))))

  ;; スクラッチバッファを表示する
  (defun my-claude-code-ide-scratch-show (buffer _project-dir)
    "Show scratch BUFFER below the leftmost window of the current frame."
    (let* ((base (frame-first-window))  ;; フレームの一番左上の live window
           (win  nil))
      (when (window-live-p base)
        (condition-case _
            (progn
              ;; 左端ウィンドウの直下に 15 行で割って表示
              (setq win (split-window base -15 'below))
              (set-window-buffer win buffer))
          (error
           ;; 何かで split に失敗しても、とにかく表示はする
           (setq win (display-buffer buffer '((display-buffer-pop-up-window)))))))
      (when (window-live-p win)
        (set-window-dedicated-p win t)
        (select-window win)
        (goto-char (point-max)))
      win))

  ;; Claude Code IDE とスクラッチバッファの起動・トグル
  (defun my-claude-code-ide-with-scratch ()
    "Toggle Claude Code IDE with scratch buffer."
    (interactive)
    (let* ((project-dir (claude-code-ide--get-working-directory))
           (claude-buffer-name (claude-code-ide--get-buffer-name project-dir))
           (claude-buffer (get-buffer claude-buffer-name))
           (claude-window (and claude-buffer (get-buffer-window claude-buffer)))
           (scratch-buffer-name (format "*claude-scratch[%s]*"
                                        (file-name-nondirectory (directory-file-name project-dir))))
           (scratch-buffer (get-buffer scratch-buffer-name))
           (scratch-window (and scratch-buffer (get-buffer-window scratch-buffer))))

      (cond
       ;; 両方が表示されている場合は両方を隠す
       ((and claude-window scratch-window)
        (claude-code-ide)  ; Claude Codeのトグル
        (delete-window scratch-window))

       ;; Claude Codeのみ表示されている場合は隠す
       (claude-window
        (claude-code-ide))  ; Claude Codeのトグル

       ;; スクラッチバッファのみ表示されている場合は両方表示
       (scratch-window
        (claude-code-ide)  ; Claude Codeを表示
        (my-claude-code-ide-scratch-show scratch-buffer project-dir)
        ;; スクラッチバッファにフォーカスを移す
        (select-window (get-buffer-window scratch-buffer)))

       ;; Claude バッファは存在するがウィンドウがない場合
       (claude-buffer
        (claude-code-ide)  ; Claude Codeを表示
        (when (or scratch-buffer (not claude-buffer))
          (my-claude-code-ide-scratch))
        (let ((scratch-win (get-buffer-window (get-buffer scratch-buffer-name))))
          (when scratch-win
            (select-window scratch-win))))

       ;; 他のケースは両方表示
       (t
        (claude-code-ide)  ; Claude Codeを表示
        ;; スクラッチバッファも表示
        (when (or scratch-buffer (not claude-buffer))
          (my-claude-code-ide-scratch))
        ;; スクラッチバッファにフォーカスを移す
        (let ((scratch-win (get-buffer-window (get-buffer scratch-buffer-name))))
          (when scratch-win
            (select-window scratch-win)))))))

  ;; 選択範囲の送信またはプロンプト入力
  (defun my-claude-code-ide-send-region-or-prompt ()
    "Send Evil selection to Claude Code or open prompt."
    (interactive)
    (if (and (bound-and-true-p evil-mode)
             (or (evil-visual-state-p)
                 (region-active-p)))
        ;; Evil visual mode での選択がある場合
        (let ((text (buffer-substring-no-properties (region-beginning) (region-end))))
          (when (evil-visual-state-p)
            (evil-exit-visual-state))
          (my-claude-code-ide-send-text text))
      ;; 選択していない場合はデフォルトの prompt コマンド
      (claude-code-ide-send-prompt)))

  ;; 数字を送信するコマンド
  (defun my-claude-code-ide-send-number-1 ()
    "Send '1' to Claude Code."
    (interactive)
    (my-claude-code-ide-send-text "1"))

  (defun my-claude-code-ide-send-number-2 ()
    "Send '2' to Claude Code."
    (interactive)
    (my-claude-code-ide-send-text "2"))

  (defun my-claude-code-ide-send-number-3 ()
    "Send '3' to Claude Code."
    (interactive)
    (my-claude-code-ide-send-text "3"))

  ;; テキストを Claude Code に送信
  (defun my-claude-code-ide-send-text (text)
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
      (message "Claude Codeに送信しました: %s" (substring text 0 (min 50 (length text))))))
  )

(provide '10-ai)
;;; 10-ai.el ends here
