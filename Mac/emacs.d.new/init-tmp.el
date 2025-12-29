;;; ~/.emacs.d/init.el --- Emacsのメイン設定ファイル -*- lexical-binding: t; -*-

;;; Commentary:

;; 最終更新: 2025-11-13


;; === 依存関係
;; [ripgrep] brew install ripgrep (全文検索系で利用)
;; [Tree-sitterをコンパイルできるもの]: xcode-select --install
;; [JDK] brew install --cask temurin21
;; [Clojure CLI] brew install clojure/tools/clojure

;; === LSP用に必要なインストール
;; [Clojure] brew install clojure-lsp/brew/clojure-lsp-native
;; [Bash] npm i -g bash-language-server
;; [C/C++] brew install llvm
;; [HTML/CSS/JSON] npm i -g vscode-langservers-extracted
;; [JavaScript/JSX/TypeScript/TSX] npm i -g typescript typescript-language-server
;; [Dockerfile] npm i -g dockerfile-language-server-nodejs
;; [YAML] npm i -g yaml-language-server
;; [Terraform] brew install terraform-ls
;; [Java] brew install jdtls
;; [Python] pipx install 'python-lsp-server[all]'
;; [Ruby] brew install solargraph
;; [Rust] rustupのインストール
;; [Zig] brew install zls
;; [SQL] go install github.com/sqls-server/sqls@latest

;; === 初回のEmacs起動後に必要なコマンド
;; M-x nerd-icons-install-fonts (nerd-iconsのフォントをインストール)
;; M-x copilot-install-server (GitHub Copilotのサーバー)

;;====================================================================
;; bookmark
;;====================================================================

(use-package bookmark
  :ensure nil
  :custom
  (bookmark-save-flag 1) ; tではなく1で毎回保存
  :config
  (defun my-bookmark-set ()
    "Set a bookmark without prompting"
    (interactive)
    (let* ((line (line-number-at-pos))
           (buf (or (file-name-nondirectory (or (buffer-file-name) ""))
                    (buffer-name)))
           (name (format "%s:L%d" (if (string-empty-p buf) (buffer-name) buf) line)))
      (bookmark-set-internal nil name 'overwrite))))

;;====================================================================
;; バッファの表示方法についての設定 (display-buffer-alist)
;;====================================================================

;; 良く使う分割方法
(defconst my-display-split
  '((display-buffer-pop-up-window
     display-buffer-use-some-window)
    (side . right)
    (window-width . 0.5)))

;; 上に書いたものが優先される
(let ((rules
       (list
        ;; flymake-show-project-diagnostics
        `("\\*Flymake diagnostics" ,@my-display-split)
        ;; vterm
        `("\\*vterm\\*" ,@my-display-split
          (window-parameters . ((dedicated . t))))
        ;; ielm
        `("\\*ielm\\*" ,@my-display-split))))
  (setq display-buffer-alist (append rules display-buffer-alist)))

(setq flymake-show-diagnostics-at-end-of-line t)

;;====================================================================
;; リネームなどで便利 (bufferfile.el)
;;====================================================================
(use-package bufferfile
  :ensure t)


;;====================================================================
;; ワークスペース (perspective.el)
;;====================================================================

(use-package perspective
  :ensure t
  :init
  (setq persp-suppress-no-prefix-key-warning t)
  (persp-mode)
  :custom
  (persp-sort 'created)
  (persp-modestring-short t))

;;====================================================================
;; ファイルツリー (Treemacs)
;;====================================================================
(use-package treemacs
  :ensure t
  :defer t
  :custom
  (treemacs-follow-mode t)
  (treemacs-project-follow-cleanup t))

;; === treemacs上でevilキーバインドを利用
(use-package treemacs-evil
  :ensure t
  :after (treemacs evil evil-collection)
  :hook
  ;; evilの基本キーバインドに上書きされてしまうので、treemacs-stateにする
  (treemacs-mode . (lambda ()
                     (when (bound-and-true-p evil-local-mode)
                       (evil-change-state 'treemacs))
                     (evil-normalize-keymaps)))
  :config
  (evil-set-initial-state 'treemacs-mode 'treemacs))

;; === 統一感のためnerd-iconsをtreemacsでも利用
(use-package treemacs-nerd-icons
  :ensure t
  :after (treemacs nerd-icons)
  :config
  (treemacs-nerd-icons-config))

;; === ワークスペース別にtreemacs切り替え
(use-package treemacs-perspective
  :ensure t
  :after (treemacs perspective)
  :config
  (treemacs-set-scope-type 'Perspectives))

;;====================================================================
;; ミニバッファ内での検索・候補選択
;;====================================================================


;; === embark-exportしたバッファを直接編集して一括置換などを実現する (wgrep)



;;====================================================================
;; GitHub連携 (forge)
;;====================================================================

(use-package forge
  :after magit
  :ensure t
  :config
  ;; GitHubのアクセストークンを取得しておくこと
  ;; ~/.authinfo.gpgにGitHubのアクセストークンを設定しておくこと
  (setq forge-owned-accounts '("chaploud")))

;;====================================================================
;; ターミナル (vterm)
;;====================================================================

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
                  ;; Claude Code IDEなどでnbspが青く可視化されるのが気に食わないため
                  (setq-local nobreak-char-display 'nil)
                  (evil-insert-state)))
  :bind
  ("C-'" . my-toggle-vterm)
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

;;====================================================================
;; Git操作 (magit・diff-hl・vc)
;;====================================================================

;; === magit
(use-package magit
  :ensure t
  :custom
  (magit-blame-echo-style 'headings)
  :config
  ;; magit-blame-echoをトグルする
  (defun my-magit-blame-echo-toggle ()
    "Toggle `magit-blame-echo'."
    (interactive)
    (if (bound-and-true-p magit-blame-mode)
        (magit-blame-quit)
      (call-interactively 'magit-blame-echo))))

;; === フリンジに差分を強調表示 (diff-hl)
(use-package diff-hl
  :ensure t
  :custom
  (global-diff-hl-mode t)
  (diff-hl-flydiff-mode t)
  :hook
  (magit-pre-refresh-hook  . diff-hl-magit-pre-refresh)
  (magit-post-refresh-hook . diff-hl-magit-post-refresh))

(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq ediff-split-window-function 'split-window-horizontally)

;;====================================================================
;; RESTクライアント (restclient.el)
;;====================================================================
(use-package restclient
  :ensure t
  :mode (("\\.http\\'"  . restclient-mode)
         ("\\.rest\\'"  . restclient-mode))
  :config
  (setq restclient-same-buffer-response t))


;;====================================================================
;; GitHub Copilot連携
;;====================================================================

;; 初回起動後にM-x copilot-install-serverが必要
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

;;====================================================================
;; Claude Code IDE
;;====================================================================

;; Claude CodeのインストールとAPIキー設定が必要
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

;;====================================================================
;; Tree-sitter
;;====================================================================

(use-package treesit
  :ensure nil
  :custom
  ;; === tree-sitterによる色付けmax
  (treesit-font-lock-level 4)
  :config
  ;; === tree-sitterの文法をインストールしたいリスト
  (setq my-treesit-language-list
        '(
          bash
          c
          clojure
          cpp
          css
          dockerfile
          groovy
          hcl
          html
          java
          javascript
          jsdoc
          json
          python
          ruby
          rust
          toml
          tsx
          typescript
          yaml
          ))
  ;; === tsの参照URLを指定(デフォルト以外)
  (setq treesit-language-source-alist
        '(
          (clojure "https://github.com/sogaiu/tree-sitter-clojure")
          (dockerfile "https://github.com/camdencheek/tree-sitter-dockerfile")
          (groovy "https://github.com/murtaza64/tree-sitter-groovy")
          (hcl "https://github.com/tree-sitter-grammars/tree-sitter-hcl")
          (toml "https://github.com/ikatyang/tree-sitter-toml")
          (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
          (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
          (yaml "https://github.com/ikatyang/tree-sitter-yaml")
          ))
  ;; === 未導入だけ自動インストール
  (defun my-treesit-install-grammars ()
    "my-treesit-language-listのうち未導入のgrammarを自動インストールする"
    (interactive)
    (dolist (lang my-treesit-language-list)
      (unless (treesit-language-available-p lang)
        (condition-case err
            (progn
              (message "[treesit] Installing grammar for %s..." lang)
              (treesit-install-language-grammar lang)
              (message "[treesit] Installed: %s..." lang))
          (error
           (message "[treesit] FAILED %s → %s" lang (error-message-string err)))))))

  (add-hook 'emacs-startup-hook
            (lambda () (run-with-idle-timer 1 nil #'my-treesit-install-grammars)))
  )

;;====================================================================
;; WebAssembly (wat/wast)
;;====================================================================
;; https://github.com/nverno/wat-ts-mode.git => ~/.emacs.d/lisp/wat-ts-mode
;; https://github.com/wasm-lsp/tree-sitter-wasm.git
(add-to-list 'load-path "~/.emacs.d/lisp/wat-ts-mode")
(require 'wat-ts-mode)
(add-to-list 'auto-mode-alist '("\\.wat\\'" . wat-ts-mode))
(add-to-list 'auto-mode-alist '("\\.wast\\'" . wat-ts-mode))

;;====================================================================
;; Format On Save設定の集約
;;====================================================================

;; === Emacs Lisp
(defun my-format-emacs-lisp ()
  "Format the current buffer as Emacs Lisp code."
  (interactive)
  (save-excursion
    (indent-region (point-min) (point-max)))
  (message "[elisp] formatted."))

;; === Clojure
(defun my-format-clojure ()
  "Format the current buffer as Clojure code using cljfmt."
  (interactive)
  ;; プロジェクトルートでのcljfmtの実行
  ;; バッファを消して再度挿入なのでsave-excursionは使えない
  (if-let* ((cljfmt-path (executable-find "cljfmt"))
            (proj (project-current))
            (project-root-path (project-root proj)))
      (let ((p-current (point))
            (default-directory project-root-path))
        (call-process-region (point-min) (point-max)
                             cljfmt-path t t nil "fix" "-" "--quiet")
        (goto-char p-current)
        (message "[cljfmt] Formatted."))
    (message "[cljfmt] skipped (formatter or project not available).")
    ))

;; === フォーマッタの適用方法をここにまとめる
;; 左: メジャーモード, 右: 優先順位をつけたフォーマット方法のリスト
(defvar my-format-rules
  '((emacs-lisp-mode . (my-format-emacs-lisp))
    (clojure-ts-mode . (:lsp my-format-clojure))
    (clojure-ts-clojurescript-mode . (:lsp my-format-clojure))
    (clojure-ts-clojurec-mode . (:lsp my-format-clojure))
    (clojure-ts-clojuredart-mode . (:lsp my-format-clojure))))

(defun my-format-try (formatter)
  "Try to format using FORMATTER."
  (pcase formatter
    (:lsp
     (when (bound-and-true-p eglot--managed-mode)
       (message "[eglot] Formatting via LSP...")
       (call-interactively #'eglot-format-buffer)
       t))

    ((and (pred fboundp) fn)
     (funcall fn)
     t)

    (_ nil)))

(defun my-format-buffer ()
  "Format the current buffer based on its major mode."
  (interactive)
  (let ((formatters (cdr (assoc major-mode my-format-rules))))
    (if (not (cl-some #'my-format-try formatters))
        (message "No suitable formatter found for %s" major-mode))))

(add-hook 'before-save-hook #'my-format-buffer)
(add-hook 'before-save-hook #'whitespace-cleanup) ;; trailing spacesの削除

;;====================================================================
;; eldoc-box
;;====================================================================
(use-package eldoc-box
  :ensure t)

;;====================================================================
;; LSP (eglot)
;;====================================================================

;; === lsp (eglot)
(use-package eglot
  :ensure nil
  :custom
  (eglot-events-buffer-config '(:size nil :format full))
  (eglot-autoshutdown t)
  (eglot-connect-timeout 360)
  (eglot-extend-to-xref nil)
  (eldoc-echo-area-use-multiline-p nil)

  :bind
  (:map eglot-mode-map
        ("C-." . eglot-code-actions))

  :hook
  ;; LSP自動起動したい場合はここに追加
  (bash-ts-mode . eglot-ensure)
  (c-ts-mode . eglot-ensure)
  (clojure-ts-mode . eglot-ensure)
  (css-ts-mode . eglot-ensure)
  (dockerfile-ts-mode . eglot-ensure)
  (html-ts-mode . eglot-ensure)
  (java-ts-mode . eglot-ensure)
  (js-ts-mode . eglot-ensure)
  (json-ts-mode . eglot-ensure)
  (python-ts-mode . eglot-ensure)
  (rust-ts-mode . eglot-ensure)
  (sql-mode . eglot-ensure)
  (terraform-mode . eglot-ensure)
  (tsx-ts-mode . eglot-ensure)
  (typescript-ts-mode . eglot-ensure)
  (yaml-ts-mode . eglot-ensure)
  (zig-mode . eglot-ensure)
  (ruby-ts-mode . eglot-ensure)

  :config
  ;; === eglotによるLSP起動
  (defun my-eglot-start ()
    "Start eglot for the current buffer if not already started."
    (interactive)
    (eglot-ensure))

  ;; デフォルトと変えたいものは指定
  (setq my-eglot-server-list
        '((sql-mode . ("sqls"))))

  (with-eval-after-load 'eglot
    (dolist (pair my-eglot-server-list)
      (add-to-list 'eglot-server-programs pair))))

;; === スニペット・テンプレート (tempel)
(use-package tempel
  :ensure t)

(use-package eglot-tempel
  :ensure t
  :init (eglot-tempel-mode t))

;; === 補完ソースの統合・拡張 (cape)
(use-package cape
  :ensure t
  :hook
  (prog-mode . my-prog-capf)
  (text-mode . my-text-capf)
  :config
  (defun my-prog-capf ()
    (unless (local-variable-p 'my-prog-capf-configured)
      (if (bound-and-true-p eglot--managed-mode)
          (setq-local completion-at-point-functions
                      (cons (cape-capf-super #'eglot-completion-at-point
                                             #'tempel-expand
                                             #'cape-file)
                            completion-at-point-functions))
        (setq-local completion-at-point-functions
                    (cons (cape-capf-super #'tempel-complete
                                           #'cape-file)
                          completion-at-point-functions))))
    (setq-local my-prog-capf-configured t))

  (defun my-text-capf ()
    (unless (local-variable-p 'my-text-capf-configured)
      (setq-local completion-at-point-functions
                  (cons (cape-capf-super #'tempel-complete
                                         #'cape-dabbrev
                                         #'cape-file)
                        completion-at-point-functions)))
    (setq-local my-text-capf-configured t))
  )

;;====================================================================
;; Markdown
;;====================================================================

(use-package markdown-mode
  :ensure t
  :mode ("\\.md\\'" . gfm-mode)
  :init
  (setq markdown-command '("pandoc" "--from=markdown" "--to=html5"))
  :bind
  (:map markdown-mode-map
        ("<backtab>" . markdown-promote)
        ("<normal-state> S-<tab>" . markdown-promote))
  :custom
  (markdown-fontify-code-blocks-natively t)
  (markdown-indent-on-enter 'indent-and-new-item)
  (markdown-gfm-use-electric-backquote nil))

;;====================================================================
;; Clojure/ClojureScript/ClojureDart
;;====================================================================
;; [依存関係]
;; brew install --cask temurin21
;; brew install clojure/tools/clojure
;; brew install clojure-lsp/brew/clojure-lsp-native

;; === clojure-ts-mode
;; 従来のclojure-modeではなくclojure-ts-modeで置き換える
;; (.clj,.cljc,.cljs,.cljd,.edn自動認識)
(use-package clojure-ts-mode
  :ensure t
  :custom
  (clojure-ts-toplevel-inside-comment-form t)
  :config
  ;; === clojure-lsp用キャッシュの削除 & 再起動
  (defun my-clojure-lsp-clear-cache-and-restart ()
    "Clear clojure-lsp cache and restart the server."
    (interactive)
    (call-interactively #'eglot-shutdown)
    (let* ((root-dir (project-root (project-current)))
           (lsp-cache (file-name-concat root-dir ".lsp/.cache"))
           (kondo-cache (file-name-concat root-dir ".clj-kondo/.cache")))
      (when (file-directory-p lsp-cache)
        (delete-directory lsp-cache t)
        (message "[%s] Deleted clojure-lsp cache at %s" (my-display-time) lsp-cache))
      (when (file-directory-p kondo-cache)
        (delete-directory kondo-cache t)
        (message "[%s] Deleted clj-kondo cache at %s" (my-display-time) kondo-cache))
      )
    (eglot-ensure)))

(use-package cider
  :ensure t
  :hook (clojure-ts-mode . cider-mode)
  :custom
  (cider-repl-buffer-size-limit 10000)
  (cider-font-lock-dynamically '(macro core function var deprecated))
  (cider-repl-pop-to-buffer-on-connect nil)
  (cider-use-xref nil)

  :config
  (defun my-cider-send-reload ()
    "Send (reload) to REPL"
    (interactive)
    (when (cider-connected-p)
      (cider-interactive-eval "(user/reload)"))))

;; 構造的編集 (puni)
(use-package puni
  :ensure t
  :demand t
  :hook
  ((emacs-lisp-mode . puni-mode)
   (lisp-interaction-mode . puni-mode)
   (clojure-ts-mode . puni-mode))
  :config
  ;; === puniでカーソル以降をそのレベルで閉じるまで削除
  (defun my-puni-kill-to-end ()
    "Kill to the end of the current sexp."
    (interactive)
    (let ((begin (point))
          (end (save-excursion (puni-end-of-sexp) (point))))
      (kill-region begin end)))

  ;; === puniでカーソル以降をそのレベルで閉じる直前までコピー
  (defun my-puni-yank-to-end ()
    "Kill to the end of the current sexp."
    (interactive)
    (let ((begin (point))
          (end (save-excursion (puni-end-of-sexp) (point))))
      (evil-yank begin end)
      (evil-goggles--show-blocking-hint begin end)))

  ;; === puniでシンボルを""で囲む
  (defun my-wrap-symbol-with-quotes ()
    "Surround the current symbol with double quotes."
    (interactive)
    (let ((bounds (bounds-of-thing-at-point 'symbol)))
      (when bounds
        (save-excursion
          (goto-char (cdr bounds))
          (insert "\"")
          (goto-char (car bounds))
          (insert "\""))))))

;; Javaライブラリのジャンプ時などに
(use-package jarchive
  :ensure t
  :after eglot
  :config
  (jarchive-setup))

;;====================================================================
;; Shell Script
;;====================================================================
;; [依存関係]
;; npm i -g bash-language-server
;; brew install shfmt

(use-package sh-script
  :ensure nil
  :mode (("\\.\\(sh\\|bash\\)\\'" . bash-ts-mode) ; sh/bash
         ("\\.?\\(bashrc\\|bash_profile\\)\\'" . bash-ts-mode) ; bash
         ("\\.?zsh\\(rc\\|env\\|profile\\)?\\'" . bash-ts-mode)) ; zsh
  :interpreter (("sh"   . bash-ts-mode)
                ("bash" . bash-ts-mode)
                ("zsh"  . bash-ts-mode))
  :custom
  (sh-basic-offset 2)
  (sh-indentation  2))

;;====================================================================
;; C言語
;;====================================================================
;; [依存関係]
;; brew install llvm
(use-package c-ts-mode
  :ensure nil
  :mode (("\\.c\\'" . c-ts-mode)
         ("\\.h\\'" . c-ts-mode)))

;;====================================================================
;; HTML/CSS/JSON
;;====================================================================
;; [依存関係]
;; npm i -g vscode-langservers-extracted

(use-package html-ts-mode
  :ensure nil
  :mode "\\.x?html\\'")

(use-package css-ts-mode
  :ensure nil
  :mode "\\.css\\'")

(use-package json-ts-mode
  :ensure nil
  :mode (("\\.json\\'" . json-ts-mode)
         ("\\.arb\\'" . arb-mode))
  :config
  (define-derived-mode arb-mode json-ts-mode "ARB"
    "Major mode for editing ARB files."
    (setq-local tab-width 4)
    (setq-local standard-indent 4)
    (setq-local json-ts-mode-indent-offset 4)
    ))

;;====================================================================
;; JavaScript / JSX / TypeScript / TSX
;;====================================================================
;; [依存関係]
;; npm i -g typescript typescript-language-server

(use-package js-ts-mode
  :ensure nil
  :mode (("\\.js\\'" . js-ts-mode)
         ("\\.cjs\\'" . js-ts-mode)
         ("\\.mjs\\'" . js-ts-mode)
         ("\\.jsx\\'" . js-ts-mode))
  :config
  (js-indent-level 2))

(use-package typescript-ts-mode
  :ensure nil
  :mode "\\.ts\\'")

(use-package tsx-ts-mode
  :ensure nil
  :mode "\\.tsx\\'")

;;====================================================================
;; Docker
;;====================================================================
;; [依存関係]
;; npm i -g dockerfile-language-server-nodejs
(use-package dockerfile-ts-mode
  :ensure nil
  :mode (("Dockerfile\\'" . dockerfile-ts-mode)
         ("\\.dockerfile\\'" . dockerfile-ts-mode)))

(use-package docker
  :ensure t
  :custom
  (docker-container-columns
   '((:name "Names" :width 30 :template "{{ json .Names }}" :sort nil :format nil)
     (:name "Status" :width 30 :template "{{ json .Status }}" :sort nil :format nil)
     (:name "Ports" :width 30 :template "{{ json .Ports }}" :sort nil :format nil)))
  (docker-container-default-sort-key '("Names")))

;; docker compose upなどでコンテナが起動していれば
;; M-x find-fileから/docker:コンテナ名:/path/to/fileで接続すればlspもうまく動作
;; コンテナのシェルにアタッチしたときに、C-p, C-nが2回必要な問題は、config.jsonに以下を記載
;; { "detachKeys": "ctrl-z,z" }

;;====================================================================
;; YAML
;;====================================================================
;; [依存関係]
;; npm i -g yaml-language-server

(use-package yaml-ts-mode
  :ensure nil
  :mode ("\\.ya?ml\\'" . yaml-ts-mode))

;;====================================================================
;; Terraform (.tf)
;;====================================================================
;; [依存関係]
;; brew install terraform-ls

(use-package terraform-mode
  :ensure t
  :mode "\\.tf\\'")

;;====================================================================
;; Java
;;====================================================================
;; [依存関係]
;; brew install jdtls

(use-package java-ts-mode
  :ensure nil
  :mode "\\.java\\'")

;;====================================================================
;; Groovy DSL (.gradle)
;;====================================================================
(use-package groovy-mode
  :ensure t
  :mode "\\.gradle\\'")

;;====================================================================
;; Python
;;====================================================================
;; [依存関係]
;; pipx install 'python-lsp-server[all]'
(use-package python-ts-mode
  :ensure nil
  :mode "\\.py\\'"
  :interpreter ("python" . python-ts-mode))

;;====================================================================
;; Ruby
;;====================================================================
;; [依存関係]
;; gem install ruby-lsp ruby-lsp-rails ruby-lsp-rspec rubocop rubocop-rails syntax_tree
(use-package ruby-ts-mode
  :ensure nil
  :mode (("\\.rb\\'" . ruby-ts-mode)
         ("\\.rake\\'" . ruby-ts-mode)
         ("Rakefile\\'" . ruby-ts-mode)
         ("Gemfile\\'" . ruby-ts-mode)))

;;====================================================================
;; Rust
;;====================================================================
;; [依存関係]
;; rustupのインストール
(use-package rust-ts-mode
  :ensure nil
  :mode "\\.rs\\'")

;;====================================================================
;; TOML
;;====================================================================
(use-package toml-ts-mode
  :ensure nil
  :mode "\\.toml\\'")

;;====================================================================
;; Zig
;;====================================================================
;; [依存関係]
;; brew install zig
;; brew install zls

(use-package zig-mode
  :ensure t
  :mode (("\\.zig\\'" . zig-mode)
         ("\\.zon\\'" . zig-mode))
  :hook
  (eglot-managed-mode-hook
   .
   (lambda ()
     (when (eq major-mode 'zig-mode)
       (eglot-inlay-hints-mode -1))))
  :config
  (setq compilation-auto-jump-to-first-error t)

  (defun my-zig-build-project ()
    (interactive)
    (let* ((proj (project-current))
           (project-root (if proj (project-root proj) default-directory))
           (default-directory project-root))
      (compile "make -k build")))

  (defun my-zig-run-project ()
    (interactive)
    (let* ((proj (project-current))
           (project-root (if proj (project-root proj) default-directory))
           (default-directory project-root))
      (compile "make -k run")))

  (defun my-zig-test-project ()
    (interactive)
    (let* ((proj (project-current))
           (project-root (if proj (project-root proj) default-directory))
           (default-directory project-root))
      (compile "make -k test"))))

;;====================================================================
;; Makefile
;;====================================================================
;; [依存関係]
;; brew install checkmake
(add-to-list 'auto-mode-alist '("\\(?:[Mm]akefile\\|\\.mk\\)\\'" . makefile-mode))
(add-hook 'makefile-mode-hook
          (lambda ()
            (setq-local indent-tabs-mode t)
            (setq-local tab-width 4)))
(add-hook 'compilation-filter-hook 'ansi-color-compilation-filter)

;;====================================================================
;; DB接続
;;====================================================================
;; [依存関係]
;; go install github.com/sqls-server/sqls@latest

(use-package sql
  :ensure nil
  :custom
  (sql-postgres-login-params nil)
  (sql-connection-alist
   '(;; SQLアンチパターン
     (sql-antipatterns
      (sql-product 'mysql)
      (sql-user "root")
      (sql-password "")
      (sql-server "localhost")
      (sql-port 3306)
      (sql-database "anti_patterns"))))
  :hook (sql-mode . (lambda () (sql-indent-enable))))

;;====================================================================
;; elfeed
;;====================================================================
(defun my-elfeed-search-print-entry (entry)
  "My Print ENTRY to the buffer."
  (let* ((date (elfeed-search-format-date (elfeed-entry-date entry)))
         (title (or (elfeed-meta entry :title) (elfeed-entry-title entry) ""))
         (title-faces (elfeed-search--faces (elfeed-entry-tags entry)))
         (title-width (- (window-width) 10 elfeed-search-trailing-width))
         (title-column (elfeed-format-column
                        title (elfeed-clamp
                               elfeed-search-title-min-width
                               title-width
                               elfeed-search-title-max-width)
                        :left)))
    (insert (propertize date 'face 'elfeed-search-date-face) " ")
    (insert (propertize title-column 'face title-faces 'kbd-help title) " ")))

(use-package elfeed
  :ensure t
  :custom
  (elfeed-search-title-min-width 40)
  (elfeed-search-title-max-width 120)
  (elfeed-search-filter "@1-week-ago +unread")
  (elfeed-search-print-entry-function #'my-elfeed-search-print-entry)
  (shr-use-fonts nil))

;;====================================================================
;; キーバインド (general.el)
;;====================================================================

;; use-packageと:generalの組み合わせで色々できる
(use-package general
  :ensure t
  :after (evil evil-collection)
  :config
  (general-evil-setup)
  (general-auto-unbind-keys)

  ;; === リーダーキー定義 (SPC)
  (general-create-definer my-global-leader-def
    :states '(normal visual)
    :keymaps 'override
    :prefix "SPC")

  ;; === ローカルリーダーキー定義 (,)
  (general-create-definer my-local-leader-def
    :states '(normal)
    :keymaps 'override
    :prefix ",")

  ;; === モーション系 (g)
  (general-create-definer my-motion-leader-def
    :states '(normal visual)
    :keymaps 'override
    :prefix "g")

  ;; === 全メジャーモード共通のキーバインド
  (my-global-leader-def
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
    "q r" '(my-reset-emacs :wk "reset emacs")

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
    "f d" '(bufferfile-delete :wk "delete file")
    "f t" '(treemacs :wk "treemacs")
    "f y" '(my-copy-project-relative-path :wk "copy relative path")
    "f Y" '(my-copy-absolute-path :wk "copy absolute path")

    ;; (n) ノート操作
    "n" '(:ignore t :wk "Notes")
    "n n" '(my-new-note :wk "new note")
    "n f" '(my-find-note :wk "find note")
    "n s" '(my-grep-note :wk "search note")

    ;; (i) init.el操作
    "i" '(:ignore t :wk "init.el")
    "i i" '(my-open-user-init :wk "open init.el")
    "i r" '(my-reload-user-init :wk "reload init.el")

    ;; (b) バッファ操作/ブックマーク
    "b" '(:ignore t :wk "Buffers/Bookmark")
    "b b" '(consult-buffer :wk "buffer switch")
    "b d" '(kill-current-buffer :wk "buffer kill")
    "b r" '(bufferfile-rename :wk "rename buffer file")
    "b h" '(dashboard-open :wk "dashboard home")
    "b l" '(consult-bookmark :wk "bookmark list")
    "b s" '(my-bookmark-set :wk "bookmark set")
    "b k" '(bookmark-delete :wk "bookmark delete")

    ;; (s) 検索
    "s" '(:ignore t :wk "Search")
    "s s" '(consult-line :wk "search in buffer")
    "s p" '(consult-ripgrep :wk "search in project")
    "s d" '(my-consult-ripgrep-current-dir :wk "search in directory")

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
    "g b" '(my-magit-blame-echo-toggle :wk "git blame")

    ;; (d) 差分/デバッグ/Docker/DB
    "d" '(:ignore t :wk "Diff/Debug/Docker/DB")
    "d d" '(diff-hl-show-hunk :wk "diff")
    "d c" '(docker-containers :wk "docker containers")
    "d b" '(sql-connect :wk "db connect")

    ;; (l) LSP (eglot) 操作
    "l" '(:ignore t :wk "LSP")
    "l s" '(my-eglot-start :wk "lsp start")

    ;; (c) cd on vterm
    "c" '(my-cd-on-vterm :wk "cd on vterm")

    ;; (h) http
    "h" '(:ignore t :wk "HTTP")
    "h g" '(my-plz-get :wk "GET")
    "h p" '(my-plz-post-region :wk "POST region")
    "h s" '(my-plz-set-bearer-token :wk "set bearer token")
    "h c" '(my-plz-clear-bearer-token :wk "clear bearer token")

    ;; (a) 生成AI系
    "a" '(:ignore t :wk "AI")
    ;; Claude Code IDE (M-RET: 改行, C-ESC: エスケープ)
    "a m" '(claude-code-ide-menu :wk "Claude menu")
    "a a" '(my-claude-code-ide-with-scratch  :wk "Claude start")
    "a b" '(my-claude-code-ide-scratch :wk "Claude scratch buffer")
    "a i" '(claude-code-ide-insert-at-mentioned :wk "Claude insert at mentioned")
    "a s" '(my-claude-code-ide-send-region-or-prompt :wk "Claude send prompt")
    "a n" '(claude-code-ide-insert-newline :wk "Claude insert newline")
    "1" '(my-claude-code-ide-send-number-1 :wk "Claude send '1'")
    "2" '(my-claude-code-ide-send-number-2 :wk "Claude send '2'")
    "3" '(my-claude-code-ide-send-number-3 :wk "Claude send '3'")
    "a e" '(claude-code-ide-send-escape :wk "Claude send escape")
    "a q" '(claude-code-ide-stop :wk "Claude stop")
    "a c" '(claude-code-ide-continue :wk "Claude continue")
    "a r" '(claude-code-ide-resume :wk "Claude resume")
    "a l" '(claude-code-ide-list-sessions :wk "Claude list sessions")
    )

  ;; LSP (eglot) 操作 (SPC l)
  (my-global-leader-def
    :keymaps '(eglot-mode-map)
    "l r" '(eglot-rename :wk "rename symbol")
    "l a" '(eglot-code-actions :wk "code actions")
    "l f" '(eglot-format :wk "format")
    "l R" '(eglot-reconnect :wk "lsp reconnect")
    "l q" '(eglot-shutdown :wk "lsp shutdown")
    )

  ;; === ジャンプなど (g系)
  (my-motion-leader-def
    "n" '(diff-hl-next-hunk :wk "next change")
    "p" '(diff-hl-previous-hunk :wk "prev change")
    "t" '(persp-next :wk "next workspace")
    "T" '(persp-prev :wk "prev workspace")
    )

  ;; === Lisp系の編集操作 (,)
  (my-local-leader-def
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
    "\"" '(my-wrap-symbol-with-quotes :wk "wrap \"\"")
    "d" '(:ignore t :wk "delete")
    "d w" '(puni-splice :wk "delete wrap")
    "k" '(my-puni-kill-to-end :wk "kill to sexp end") ; noramlでC-kはkill-line
    "y" '(my-puni-yank-to-end :wk "yank to sexp end")
    "h" '(eldoc :wk "eldoc")
    )

  ;; === Clojure (SPC m)
  (my-global-leader-def
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
    "l F" '(my-clojure-lsp-clear-cache-and-restart :wk "lsp clear cache/restart")
    )

  ;; === Clojure (,)
  (my-local-leader-def
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
    "R" '(my-cider-send-reload :wk "send (reload) to repl"))

  ;; === Emacs Lisp (,)
  (my-local-leader-def
    :keymaps '(emacs-lisp-mode-map
               lisp-interaction-mode-map)
    "'" '(ielm :wk "ielm")
    "e" '(:ignore t :wk "Eval")
    "e e" '(eval-last-sexp :wk "eval last sexp")
    "e f" '(eval-defun :wk "eval defun")
    "e b" '(eval-buffer :wk "eval buffer")
    "P" '(my-package-built-in-p :wk "check package built-in")
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

  ;; === Zig (,)
  (my-local-leader-def
    :keymaps '(zig-mode-map)
    "b" '(my-zig-build-project :wk "build")
    "r" '(my-zig-run-project :wk "run")
    "t" '(my-zig-test-project :wk "test")
    "f" '(zig-format-buffer :wk "format"))

  ;; === Markdown
  (my-local-leader-def
    :keymaps '(gfm-mode-map)
    "," '(markdown-toggle-gfm-checkbox :wk "toggle checkbox"))

  ;; === Flymakeのエラージャンプ
  (general-define-key
   :states '(normal)
   "] ]" '(flymake-goto-next-error :wk "goto next error")
   "[ [" '(flymake-goto-prev-error :wk "goto prev error"))

  ;; === ミニバッファでパスならスラッシュまで削除
  (general-define-key
   :states '(insert)
   :keymaps '(minibuffer-mode-map)
   "C-w" 'backward-kill-sexp
   "C-S-w" 'my-minibuffer-up-to-project-root)

  ;; === isearch(C-sまたは/)中に単語削除はM-eの後にC-DELを押すしかない
  ;; 基本はC-hで納得しよう

  )

;;====================================================================
;; 設定・色の細かいカスタマイズ
;;====================================================================

;; customizeメニューから変更した場合自動で追記・更新される
;; パッケージ固有の設定変数はuse-packageの:customで設定する方が宣言的で良い
;; 色の調整はここにまとめておいた方が見通しが良い(現在： catppuccin-macchiato を前提に調整)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(elfeed-feeds
   '("https://www.reddit.com/r/emacs/.rss"
     "https://qiita.com/tags/Emacs/feed.atom"
     "https://planet.emacslife.com/atom.xml"
     "https://developertoarchitect.com/lessons/index.xml"
     "https://qiita.com/tags/webassembly/feed"
     "https://qiita.com/tags/TypeScript/feed"
     "https://qiita.com/tags/Vim/feed.atom"
     "https://qiita.com/popular-items/feed"
     "https://rss.itmedia.co.jp/rss/2.0/techtarget.xml"
     "https://news.google.com/news/rss/search?q=%22Zig%20Language%22%20OR%20%22ziglang%22&hl=en"
     "https://zenn.dev/feed"
     "https://codezine.jp/rss/new/20/index.xml"
     "https://news.google.com/news/rss/search?q=%22wasm%22%20OR%20%22WASM%22%20OR%20%22WebAssembly%22%20OR&hl=ja-JP&gl=JP&ceid=JP:ja"
     "https://planet.clojure.in/atom.xml"
     "https://speakerdeck.com/c/programming.atom"
     "https://speakerdeck.com/c/technology.atom"
     "https://realtime.jser.info/feed.xml"
     "https://www.publickey1.jp/atom.xml"))
 '(package-selected-packages
   '(agent-shell bufferfile cape catppuccin-theme cider claude-code-ide
                 clojure-ts-mode colorful-mode consult-gh copilot
                 corfu dashboard ddskk diff-hl docker doom-modeline
                 doom-themes eglot-tempel eldoc-box elfeed
                 embark-consult evil-anzu evil-collection
                 evil-commentary evil-escape evil-goggles evil-numbers
                 evil-surround exec-path-from-shell forge general
                 groovy-mode helpful hl-todo jarchive marginalia
                 nerd-icons-corfu orderless plz puni
                 rainbow-delimiters restclient spacemacs-theme
                 terraform-mode treemacs-evil treemacs-nerd-icons
                 treemacs-perspective ultra-scroll undo-fu
                 undo-fu-session vertico vterm wgrep zig-mode))
 '(package-vc-selected-packages
   '((claude-code-ide :url
                      "https://github.com/manzaltu/claude-code-ide.el")
     (copilot :url "https://github.com/copilot-emacs/copilot.el"
              :branch "main")))
 '(safe-local-variable-values
   '((cider-connect-default-cljs-params :host "localhost" :port 9501)
     (cider-connect-default-params :host "localhost" :port 9500)
     (cider-path-translations
      ("/usr/local/eboshigara" . "~/Studist/teachme_eboshigara")
      ("/root/.m2" . "~/.m2"))
     (cider-connect-default-cljs-params :host "localhost" :port 9631)
     (cider-connect-default-params :host "localhost" :port 42004))))


;;; init.el ends here
