;;; 06-terminal.el --- ターミナルエミュレータ -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;; vterm基本設定
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
                  (setq-local nobreak-char-display nil)
                  (setq buffer-face-mode-face '(:family "UDEV Gothic 35NF" :height 130))
                  (buffer-face-mode t)
                  (evil-insert-state)))
  :bind
  (:map vterm-mode-map
        ("M-:" . eval-expression)))

;; 複数vtermインスタンス管理
(use-package multi-vterm
  :ensure t
  :bind
  ("C-'" . multi-vterm-project)
  :config
  (defun my-cd-on-vterm ()
    "Change directory of current vterm to the buffer's directory."
    (interactive)
    (let ((dir (if buffer-file-name
                   (file-name-directory buffer-file-name)
                 default-directory))
          (vterm-buffer (seq-find (lambda (buf)
                                    (with-current-buffer buf
                                      (derived-mode-p 'vterm-mode)))
                                  (buffer-list))))
      (if vterm-buffer
          (with-current-buffer vterm-buffer
            (vterm-send-string (format "cd %s" (shell-quote-argument dir)))
            (vterm-send-return)
            (message "vterm: cd %s" dir))
        (user-error "vterm バッファが見つかりません")))))

(provide '06-terminal)
;;; 06-terminal.el ends here
