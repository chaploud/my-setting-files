;;; 99-keybinds.el --- キーバインド -*- lexical-binding: t; -*-

;;; Commentary:
;; general.el によるキーバインド設定

;;; Code:

;; グローバルキーバインド
(global-set-key (kbd "C-'") #'my-vterm-toggle)

;; general.el
(use-package general
  :ensure t
  :config
  (general-evil-setup)

  ;; SPC リーダー
  (general-create-definer my-leader
    :states '(normal visual)
    :keymaps 'override
    :prefix "SPC")

  ;; , ローカルリーダー
  (general-create-definer my-local
    :states '(normal)
    :keymaps 'override
    :prefix ",")

  ;; 全体キーバインド
  (my-leader
    "SPC" '(execute-extended-command :wk "M-x")
    ";" '(evil-commentary-line :wk "comment")

    ;; (t) Toggle / Terminal
    "t" '(:ignore t :wk "Toggle/Terminal")
    "t l" '(toggle-truncate-lines :wk "truncate lines")
    "t c" '(copilot-mode :wk "copilot")
    "t n" '(my-vterm-new :wk "terminal new")

    ;; (q) Quit
    "q" '(:ignore t :wk "Quit")
    "q q" '(save-buffers-kill-terminal :wk "quit")

    ;; (f) File
    "f" '(:ignore t :wk "File")
    "f f" '(find-file :wk "find")
    "f r" '(consult-recent-file :wk "recent")
    "f p" '(project-find-file :wk "project")
    "f s" '(save-buffer :wk "save")
    "f t" '(treemacs :wk "treemacs")
    "f d" '(bufferfile-delete :wk "delete")

    ;; (b) Buffer/Bookmark
    "b" '(:ignore t :wk "Buffer")
    "b b" '(consult-buffer :wk "switch")
    "b d" '(kill-current-buffer :wk "kill")
    "b r" '(bufferfile-rename :wk "rename")
    "b h" '(dashboard-open :wk "home")
    "b l" '(consult-bookmark :wk "bookmark list")

    ;; (s) Search
    "s" '(:ignore t :wk "Search")
    "s s" '(consult-line :wk "buffer")
    "s p" '(consult-ripgrep :wk "project")

    ;; (p) Project
    "p" '(:ignore t :wk "Project")
    "p p" '(project-switch-project :wk "switch")
    "p f" '(consult-flymake :wk "flymake")

    ;; (w) Workspace/Window
    "w" '(:ignore t :wk "Workspace")
    "w w" '(persp-switch :wk "switch")
    "w r" '(persp-rename :wk "rename")
    "w d" '(persp-kill :wk "delete")
    "w u" '(winner-undo :wk "undo")

    ;; (g) Git
    "g" '(:ignore t :wk "Git")
    "g s" '(magit-status :wk "status")
    "g l" '(magit-log-current :wk "log")
    "g b" '(my-magit-blame-toggle :wk "blame")
    "g d" '(diff-hl-show-hunk :wk "diff hunk")

    ;; (l) LSP
    "l" '(:ignore t :wk "LSP")
    "l r" '(eglot-rename :wk "rename")
    "l a" '(eglot-code-actions :wk "actions")
    "l f" '(eglot-format :wk "format")
    "l R" '(eglot-reconnect :wk "reconnect")

    ;; (e) Elfeed
    "e" '(:ignore t :wk "Elfeed")
    "e f" '(elfeed :wk "open")
    "e u" '(elfeed-update :wk "update")

    ;; (d) Docker
    "d" '(:ignore t :wk "Docker")
    "d c" '(docker-containers :wk "containers")

    ;; (a) AI
    "a" '(:ignore t :wk "AI")
    "a a" '(my-claude-code :wk "claude")
    "a s" '(my-claude-send-region :wk "send region"))

  ;; Lisp系ローカルキーバインド
  (my-local
    :keymaps '(emacs-lisp-mode-map clojure-ts-mode-map)
    "s" '(puni-slurp-forward :wk "slurp")
    "b" '(puni-barf-forward :wk "barf")
    "r" '(puni-raise :wk "raise")
    "(" '(puni-wrap-round :wk "wrap ()"))

  ;; Clojure
  (my-local
    :keymaps 'clojure-ts-mode-map
    "e" '(:ignore t :wk "Eval")
    "e e" '(cider-eval-last-sexp :wk "sexp")
    "e b" '(cider-eval-buffer :wk "buffer")
    "i" '(cider-jack-in :wk "jack-in")
    "t" '(cider-switch-to-repl-buffer :wk "repl"))

  ;; Emacs Lisp
  (my-local
    :keymaps 'emacs-lisp-mode-map
    "e" '(:ignore t :wk "Eval")
    "e e" '(eval-last-sexp :wk "sexp")
    "e b" '(eval-buffer :wk "buffer"))

  ;; Flymakeエラージャンプ
  (general-define-key
   :states '(normal)
   "] ]" 'flymake-goto-next-error
   "[ [" 'flymake-goto-prev-error)

  ;; diff-hlジャンプ
  (general-define-key
   :states '(normal)
   :prefix "g"
   "n" 'diff-hl-next-hunk
   "p" 'diff-hl-previous-hunk
   "t" 'persp-next
   "T" 'persp-prev))

(provide '99-keybinds)
;;; 99-keybinds.el ends here
