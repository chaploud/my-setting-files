;;; 09-ai.el --- AI連携 -*- lexical-binding: t; -*-

;;; Commentary:
;; GitHub Copilot, Claude Code (vterm経由)

;;; Code:

;; copilot (初回: M-x copilot-install-server)
(use-package copilot
  :ensure t
  :vc (:url "https://github.com/copilot-emacs/copilot.el" :branch "main")
  :hook (prog-mode . copilot-mode)
  :bind (:map copilot-completion-map
              ("C-<tab>" . copilot-accept-completion))
  :custom (copilot-max-char 1000000)
  :config
  (dolist (mode '(prog-mode org-mode text-mode emacs-lisp-mode))
    (add-to-list 'copilot-indentation-alist `(,mode 2))))

;; Claude Code (vterm経由のシンプル実装)
(defun my-claude-code ()
  "Start Claude Code in project directory."
  (interactive)
  (let* ((proj (project-current))
         (dir (if proj (project-root proj) default-directory))
         (proj-name (file-name-nondirectory (directory-file-name dir)))
         (buf-name (format "*claude[%s]*" proj-name))
         (buf (get-buffer buf-name)))
    (if buf
        (pop-to-buffer buf)
      (let ((default-directory dir))
        (vterm buf-name)
        (run-at-time 0.5 nil
                     (lambda ()
                       (when (get-buffer buf-name)
                         (with-current-buffer buf-name
                           (vterm-send-string "claude")
                           (vterm-send-return)))))))))

(defun my-claude-send (text)
  "Send TEXT to Claude Code."
  (interactive "sText: ")
  (let* ((proj (project-current))
         (dir (if proj (project-root proj) default-directory))
         (proj-name (file-name-nondirectory (directory-file-name dir)))
         (buf-name (format "*claude[%s]*" proj-name))
         (buf (get-buffer buf-name)))
    (if buf
        (with-current-buffer buf
          (vterm-send-string text)
          (vterm-send-return))
      (message "Claude Code not running. Start with my-claude-code first."))))

(defun my-claude-send-region ()
  "Send region to Claude Code."
  (interactive)
  (if (use-region-p)
      (my-claude-send (buffer-substring-no-properties (region-beginning) (region-end)))
    (message "No region selected.")))

(provide '09-ai)
;;; 09-ai.el ends here
