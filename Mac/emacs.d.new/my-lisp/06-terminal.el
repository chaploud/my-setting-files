;;; 06-terminal.el --- ターミナルエミュレータ -*- lexical-binding: t; -*-

;;; Commentary:
;;; Code:

(use-package vterm
  :ensure t
  :after evil
  :custom
  (vterm-max-scrollback 100000)
  (vterm-tramp-shells '(("ssh" "/bin/bash")
                        ("scp" "/bin/bash")
                        ("docker" "/bin/bash")))
  :hook
  (vterm-mode . (lambda ()
                  ;; nbspの不可視化
                  (setq-local nobreak-char-display 'nil)
                  (evil-insert-state)))
  :bind
  ("C-'" . my-toggle-vterm) ;; terminalインスタンスとトグルをこだわって調整
  (:map vterm-mode-map
        ("M-:" . eval-expression))
  :config
  ;; vterm-toggleパッケージは使わない(claude-code-ideとの兼ね合い)
  (defun my-toggle-vterm ()
    "Toggle vterm terminal."
    (interactive)
    (let* ((vterm-buffer (get-buffer "*vterm*"))
           (vterm-window (get-buffer-window "*vterm*"))
           (current-project-root (or (and (project-current)
                                          (project-root (project-current)))
                                     default-directory))
           (vterm-dir (when vterm-buffer
                        (with-current-buffer vterm-buffer
                          default-directory))))
      (if (and vterm-window (eq (selected-window) vterm-window))
          (quit-window)
        (let* ((default-directory current-project-root)
               (vterm-buf (vterm)))
          (with-current-buffer vterm-buf)
          (evil-insert-state))
        ;; 現在バッファのプロジェクトが異なるなら、気を利かせてディレクトリ移動する
        (when (and vterm-dir
                   (not (string= (file-truename current-project-root)
                                 (file-truename vterm-dir))))
          (vterm-send-string (format "cd %s" current-project-root))
          (vterm-send-return)
          (with-current-buffer vterm-buffer
            (setq-local default-directory current-project-root))))))

  (defun my-cd-on-vterm ()
    "Change directory of vterm to the current buffer's parent directory."
    (interactive)
    (let ((dir (if buffer-file-name
                   (file-name-directory buffer-file-name)
                 default-directory)))
      (with-current-buffer (get-buffer "*vterm*")
        (vterm-send-string (format "cd %s" dir))
        (vterm-send-return)))))

(provide '06-terminal)
;;; 06-terminal.el ends here
