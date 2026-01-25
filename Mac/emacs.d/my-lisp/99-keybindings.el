;;; 99-keybindings.el --- キーバインド -*- lexical-binding: t; -*-

;;; Commentary:
;; general.el によるキーバインド設定

;;; Code:

(use-package general
  :ensure t
  :after (evil evil-collection)
  :config
  (general-evil-setup)
  (general-auto-unbind-keys)

  ;; === リーダーキー定義 (SPC)
  (general-create-definer my/global-leader-def
    :states '(normal visual)
    :keymaps 'override
    :prefix "SPC")

  ;; === ローカルリーダーキー定義 (,)
  (general-create-definer my/local-leader-def
    :states '(normal)
    :keymaps 'override
    :prefix ",")

  ;; === モーション系 (g)
  (general-create-definer my/motion-leader-def
    :states '(normal visual)
    :keymaps 'override
    :prefix "g")

  ;; === 全メジャーモード共通のキーバインド
  (my/global-leader-def
    ;; (SPC) M-x
    "SPC" '(execute-extended-command :wk "M-x")

    ;; (;) コメント
    ";" '(evil-commentary-line :wk "comment")

    ;; (t) トグル
    "t" '(:ignore t :wk "Toggle")
    "t l" '(toggle-truncate-lines :wk "truncate line")
    "t f" '(flymake-mode :wk "toggle flymake")
    "t c" '(copilot-mode :wk "toggle copilot")
    "t h" '(eldoc-box-hover-at-point-mode :wk "toggle eldoc-box 1")
    "t H" '(eldoc-box-hover-mode :wk "toggle eldoc-box 2")

    ;; (q) 終了操作
    "q" '(:ignore t :wk "Quit")
    "q q" '(save-buffers-kill-terminal :wk "quit")
    "q r" '(my/reset-emacs :wk "reset emacs")

    ;; (e) elfeed
    "e" '(:ignore t :wk "elfeed")
    "e f" '(elfeed :wk "elfeed")
    "e r" '(elfeed-update :wk "elfeed update")

    ;; (f) ファイル操作
    "f" '(:ignore t :wk "Files")
    "f f" '(find-file :wk "file find")
    "f r" '(consult-recent-file :wk "file recent")
    "f p" '(project-find-file :wk "find in project")
    "f s" '(save-buffer :wk "file save")
    "f t" '(treemacs :wk "treemacs")
    "f y" '(my/copy-project-relative-path :wk "copy relative path")
    "f Y" '(my/copy-absolute-path :wk "copy absolute path")

    ;; (n) ノート操作
    "n" '(:ignore t :wk "Notes")
    "n n" '(my/new-note :wk "new note")
    "n f" '(my/find-note :wk "find note")
    "n s" '(my/grep-note :wk "search note")

    ;; (i) init.el操作
    "i" '(:ignore t :wk "init.el")
    "i i" '(my/open-user-init :wk "open init.el")
    "i r" '(my/reload-user-init :wk "reload init.el")

    ;; (b) バッファ操作/ブックマーク
    "b" '(:ignore t :wk "Buffers/Bookmark")
    "b b" '(consult-buffer :wk "buffer switch")
    "b d" '(kill-current-buffer :wk "buffer kill")
    "b k" '(bufferfile-delete :wk "file delete")
    "b r" '(bufferfile-rename :wk "file rename")
    "b h" '(dashboard-open :wk "dashboard home")
    "b l" '(consult-bookmark :wk "bookmark list")
    "b s" '(my/bookmark-set :wk "bookmark set")
    "b x" '(bookmark-delete :wk "bookmark delete")

    ;; (s) 検索
    "s" '(:ignore t :wk "Search")
    "s s" '(consult-line :wk "search in buffer")
    "s p" '(consult-ripgrep :wk "search in project")
    "s d" '(my/consult-ripgrep-current-dir :wk "search in directory")

    ;; (p) プロジェクト管理
    "p" '(:ignore t :wk "Project/Package")
    "p p" '(project-switch-project :wk "project switch")
    "p f" '(consult-flymake :wk "project flymake")

    ;; (w) ワークスペース/ウィンドウ操作
    "w" '(:ignore t :wk "Workspace/Window")
    "w w" '(persp-switch :wk "workspace switch")
    "w r" '(persp-rename :wk "workspace rename")
    "w d" '(persp-kill :wk "workspace kill")
    "w s" '(persp-state-save :wk "workspace save")
    "w l" '(persp-state-load :wk "workspace load")
    "w u" '(winner-undo :wk "window undo")

    ;; (g) Git/ジャンプ
    "g" '(:ignore t :wk "Git/GoTo")
    "g s" '(magit-status-quick :wk "git status")
    "g l" '(magit-log-current :wk "git log")
    "g d" '(vc-diff :wk "git diff")
    "g b" '(my/magit-blame-echo-toggle :wk "git blame")

    ;; (d) 差分/デバッグ/Docker/DB
    "d" '(:ignore t :wk "Diff/Debug/Docker/DB")
    "d d" '(diff-hl-show-hunk :wk "diff")
    "d c" '(docker-containers :wk "docker containers")
    "d b" '(sql-connect :wk "db connect")

    ;; (l) LSP (eglot) 操作
    "l" '(:ignore t :wk "LSP")
    "l s" '(my/eglot-start :wk "lsp start")

    ;; (c) cd on vterm
    "c" '(my/cd-on-vterm :wk "cd on vterm")

    ;; (a) Claude Code IDE
    "a" '(:ignore t :wk "AI/Claude")
    "a a" '(my/claude-code-ide :wk "Claude toggle")
    "a m" '(claude-code-ide-menu :wk "Claude menu")
    "a s" '(my/claude-send-region :wk "Claude send region")
    "a p" '(claude-code-ide-send-prompt :wk "Claude prompt")
    "a e" '(claude-code-ide-send-escape :wk "Claude ESC")
    "a q" '(claude-code-ide-stop :wk "Claude stop")
    "a c" '(claude-code-ide-continue :wk "Claude continue")
    "a r" '(my/claude-code-ide-resume :wk "Claude resume")
    "a b" '(my/claude-scratch :wk "Claude scratch")
    "a n" '(my/claude-send-return :wk "Claude Enter")
    "a TAB" '(my/claude-send-shift-tab :wk "Claude Shift+Tab")
    "a w" '(my/claude-send-rewind :wk "Claude Rewind")
    "1" '(my/claude-send-1 :wk "Claude '1'")
    "2" '(my/claude-send-2 :wk "Claude '2'")
    "3" '(my/claude-send-3 :wk "Claude '3'")
    "4" '(my/claude-send-4 :wk "Claude '4'")
    )

  ;; LSP (eglot) 操作 (SPC l)
  (my/global-leader-def
    :keymaps '(eglot-mode-map)
    "l r" '(eglot-rename :wk "rename symbol")
    "l a" '(eglot-code-actions :wk "code actions")
    "l f" '(eglot-format :wk "format")
    "l R" '(eglot-reconnect :wk "lsp reconnect")
    "l q" '(eglot-shutdown :wk "lsp shutdown")
    )

  ;; === ジャンプなど (g系)
  (my/motion-leader-def
    "n" '(diff-hl-next-hunk :wk "next change")
    "p" '(diff-hl-previous-hunk :wk "prev change")
    "t" '(persp-next :wk "next workspace")
    "T" '(persp-prev :wk "prev workspace")
    )

  ;; === Lisp系の編集操作 (,)
  (my/local-leader-def
    :keymaps '(emacs-lisp-mode-map
               lisp-interaction-mode-map
               clojure-ts-mode-map
               clojure-ts-clojurescript-mode-map
               clojure-ts-clojurec-mode-map
               clojure-ts-clojuredart-mode-map)
    "s" '(puni-slurp-forward :wk "slurp forward")
    "S" '(puni-slurp-backward :wk "slurp backward")
    "b" '(puni-barf-forward :wk "barf forward")
    "B" '(puni-barf-backward :wk "barf backward")
    "r" '(puni-raise :wk "raise sexp")
    "p" '(puni-splice-killing-backward :wk "splice backward")
    "(" '(puni-wrap-round :wk "wrap ()")
    "[" '(puni-wrap-square :wk "wrap []")
    "{" '(puni-wrap-curly :wk "wrap {}")
    "\"" '(my/wrap-symbol-with-quotes :wk "wrap \"\"")
    "d" '(:ignore t :wk "delete")
    "d w" '(puni-splice :wk "delete wrap")
    "k" '(my/puni-kill-to-end :wk "kill to sexp end") ; normalでC-kはkill-line
    "y" '(my/puni-yank-to-end :wk "yank to sexp end")
    "h" '(eldoc :wk "eldoc")
    )

  ;; === Clojure (SPC m)
  (my/global-leader-def
    :keymaps '(clojure-ts-mode-map
               clojure-ts-clojurescript-mode-map
               clojure-ts-clojurec-mode-map
               clojure-ts-clojuredart-mode-map)
    "m" '(:ignore t :wk "Clojure")
    "m i" '(cider-jack-in :wk "cider jack-in")
    "m c" '(:ignore t :wk "cider connect")
    "m c c" '(cider-connect-clj&cljs :wk "connect clj&cljs")
    "m c j" '(cider-connect-clj :wk "connect clj")
    "m c s" '(cider-connect-cljs :wk "connect cljs")
    "m q" '(cider-quit :wk "cider quit")
    "l F" '(my/clojure-lsp-clear-cache-and-restart :wk "lsp clear cache/restart")
    )

  ;; === Clojure (,)
  (my/local-leader-def
    :keymaps '(clojure-ts-mode-map
               clojure-ts-clojurescript-mode-map
               clojure-ts-clojurec-mode-map
               clojure-ts-clojuredart-mode-map)
    "e" '(:ignore t :wk "Eval")
    "e e" '(cider-eval-last-sexp :wk "eval last sexp")
    "e f" '(cider-eval-dwim :wk "eval dwim")
    "e b" '(cider-eval-buffer :wk "eval bufer")
    "e n" '(cider-eval-ns-form :wk "eval ns form")
    "i" '(cider-insert-defun-in-repl :wk "insert to repl")
    "n" '(:ignore t :wk "Namespace")
    "n r" '(cider-ns-refresh :wk "cider ns refresh")
    "n s" '(cider-repl-set-ns :wk "cider ns set")
    "t" '(cider-switch-to-repl-buffer :wk "cider switch to repl")
    "R" '(my/cider-send-reload :wk "send (reload) to repl"))

  ;; === Emacs Lisp (,)
  (my/local-leader-def
    :keymaps '(emacs-lisp-mode-map
               lisp-interaction-mode-map)
    "'" '(ielm :wk "ielm")
    "e" '(:ignore t :wk "Eval")
    "e e" '(eval-last-sexp :wk "eval last sexp")
    "e f" '(eval-defun :wk "eval defun")
    "e b" '(eval-buffer :wk "eval buffer")
    "P" '(my/package-built-in-p :wk "check package built-in")
    )

  ;; === Emacs Lispの便利ヘルプ
  (general-define-key
   :keymaps 'emacs-lisp-mode-map
   :states '(normal)
   "K" 'helpful-at-point)

  ;; === Clojureのキーバインド強制上書き
  (general-define-key
   :keymaps '(cider-mode-map)
   :states '(normal)
   "K" 'eldoc)

  ;; === Markdown
  (my/local-leader-def
    :keymaps '(gfm-mode-map)
    "," '(markdown-toggle-gfm-checkbox :wk "toggle checkbox"))

  ;; Markdown promote (normal state での S-<tab>)
  (general-define-key
   :keymaps '(markdown-mode-map gfm-mode-map)
   :states '(normal)
   "S-<tab>" 'markdown-promote)

  ;; === Flymakeのエラージャンプ
  (general-define-key
   :states '(normal)
   "] ]" '(flymake-goto-next-error :wk "goto next error")
   "[ [" '(flymake-goto-prev-error :wk "goto prev error"))

  ;; === ミニバッファでパスならスラッシュまで削除
  (general-define-key
   :states '(insert)
   :keymaps '(minibuffer-mode-map)
   "C-w" 'my/minibuffer-backward-delete-path-segment
   "C-S-w" 'my/minibuffer-up-to-project-root)

  ;; === isearch(C-sまたは/)中に単語削除はM-eの後にC-DELを押すしかない
  ;; 基本はC-hで納得しよう

  )

(provide '99-keybindings)
;;; 99-keybindings.el ends here
