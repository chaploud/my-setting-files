;;; 98-utilities.el --- ユーティリティ関数 -*- lexical-binding: t; -*-

;;; Commentary:
;; Emacs基本設定、環境変数、サーバー

;;; Code:

(defun my-open-user-init ()
  "Open the user's init file."
  (interactive)
  (find-file user-init-file))

(defun my-reload-user-init ()
  "Reload the user's init file."
  (interactive)
  (load-file user-init-file)
  (message "Reloaded %s" user-init-file))

;; === カーソル下のシンボルが組み込みのパッケージかどうかチェック
(defun my-package-built-in-p (symbol)
  "Check if SYMBOL is a built-in package."
  (interactive (list (or (symbol-at-point)
                         (intern (read-string "Package: ")))))
  (message "[%s] built-in: %s" symbol (when (package-built-in-p symbol) t)))

;; === 現在のファイルのプロジェクトルートからのパスをコピー
(defun my-copy-project-relative-path ()
  "Copy the current file's path relative to the project root."
  (interactive)
  (if-let* ((proj (project-current))
            (root (project-root proj))
            (file (buffer-file-name)))
      (let ((relpath (file-relative-name file root)))
        (kill-new relpath)
        (message "Copied: %s" relpath))
    (user-error "Not visiting a file in a project.")))

(defun my-copy-absolute-path ()
  "Copy the current file's absolute path."
  (interactive)
  (if-let ((file (buffer-file-name)))
      (progn
        (kill-new file)
        (message "Copied absolute path: %s" file))
    (user-error "Not visiting a file.")))

(defun my-reset-emacs ()
  "Kill all perspectives except the current one, and kill all buffers except Eglot buffers."
  (interactive)
  ;; dashboard開く
  (dashboard-open)

  ;; 他のperspectiveを削除
  (let ((self (persp-current-name))) ; persp-kill-othersから拝借
    (cl-loop for p in (persp-names)
             when (not (string-equal p self)) do
             (persp-kill p))

    ;; 今のperspective名がmainでなければmainにリネーム
    (when (not (string-equal self "main"))
      (persp-rename "main")))

  ;; 他のバッファを全て削除
  (cl-loop for buf in (persp-current-buffers) ; persp-kill-other-buffersから拝借
           unless (or (eq buf (current-buffer))
                      (eq buf (get-buffer (persp-scratch-buffer))))
           do (kill-buffer buf))

  ;; *EGLOT プロセスバッファを削除
  (dolist (buf (buffer-list))
    (when (string-match-p "^\\*EGLOT" (buffer-name buf))
      (kill-buffer buf)))

  ;; ウィンドウ分割してない状態に
  (delete-other-windows)

  (message "(my-rest-emacs) done."))

(defvar my-notes-root (expand-file-name "~/Documents/MyNote/"))
(defvar my-inbox-dir (expand-file-name "00_inbox/" my-notes-root))
(defun my-get-next-inbox-number ()
  (let* ((files (directory-files my-inbox-dir nil "\\`[0-9]\\{3\\}_.*\\.md\\'"))
         (numbers (mapcar (lambda (f)
                            (string-to-number (substring f 0 3)))
                          files))
         (max-number (if numbers (apply 'max numbers) 0)))
    (format "%03d" (1+ max-number))))

(defun my-new-note (title)
  "Create a new inbox note with an incremented number."
  (interactive "sNote title: ")
  (let* ((next-number (my-get-next-inbox-number))
         (safe-title (replace-regexp-in-string "[^[:alnum:]-_]" "_" title))
         (filename (concat next-number "_" safe-title ".md"))
         (filepath (expand-file-name filename my-inbox-dir))
         (timestamp (format-time-string "%Y-%m-%d %H:%M")))
    (find-file filepath)
    (unless (file-exists-p filepath)
      (insert (format "# %s\n\n" title))
      (insert (format "最終更新: %s \n\n" timestamp))
      (save-buffer))
    (message "Created note: %s" filepath)))

(defun my-find-note ()
  "Search notes in my-notes-root using ripgrep and open the selected one."
  (interactive)
  (let ((default-directory my-notes-root))
    (call-interactively 'project-find-file)))

(defun my-grep-note ()
  "Grep notes in my-notes-root using ripgrep and open the selected one."
  (interactive)
  (let ((default-directory my-notes-root))
    (call-interactively 'consult-ripgrep)))

(defun my-minibuffer-up-to-project-root ()
  "When minibuffer contains a file path, replace it with the project root path."
  (interactive)
  (let* ((input (minibuffer-contents-no-properties))
         (proj (project-current))
         (root (and proj (project-root proj))))
    (if (not root)
        (user-error "No project root found")
      (cond
       ((> (length input) 0)
        (delete-minibuffer-contents)
        (insert root))
       (t
        (user-error "No path in minibuffer"))))))

;;; 98-utilities.el ends here
