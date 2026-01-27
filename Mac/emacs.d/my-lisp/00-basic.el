;;; 00-basic.el --- 基本設定 -*- lexical-binding: t; -*-

;;; Commentary:
;; 外部依存:
;; - brew install pass gpg (auth-source用)
;;
;; ~/.zshrc に以下を追加するとemacsclientで素早く開ける:
;; e() {
;;   if emacsclient --eval "t" > /dev/null 2>&1; then
;;     emacsclient -n "$@"
;;   else
;;     emacs "$@" &
;;   fi
;; }

;;; Code:

;; シェル環境変数をGUIでも利用
(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize))

;; Emacs基本設定
(use-package emacs
  :ensure nil
  :custom
  (enable-recursive-minibuffers t)      ; 再帰的ミニバッファを許可
  (mac-command-modifier 'meta)          ; CommandキーをMetaに (Mac)
  (indent-tabs-mode nil)                ; タブは空白
  (tab-width 2)                         ; タブ幅2
  (standard-indent 2)                   ; 標準インデント幅2
  (truncate-lines t)                    ; デフォルトでは行を折り返さない
  (electric-pair-mode t)                ; 自動で括弧補完
  (confirm-kill-processes nil)          ; プロセス終了確認を無効化
  (vc-follow-symlinks t)                ; シンボリックリンクを自動でたどる
  (use-short-answers t)                 ; yes/noをy/nに
  (use-file-dialog nil)                 ; ファイル選択ダイアログを使わない
  (global-so-long-mode t)               ; 大きなファイルでの動作改善
  (delete-by-moving-to-trash t)         ; ファイル削除をゴミ箱へ移動
  (global-auto-revert-mode t)           ; 変更されたファイルを自動再読み込み
  (recentf-mode t)                      ; 最近開いたファイルの履歴を保存
  (recentf-max-saved-items 50)          ; 最近開いたファイルの履歴数
  (savehist-mode t)                     ; ミニバッファ履歴保存
  (winner-mode t)                       ; ウィンドウ構成の復元
  (initial-major-mode 'text-mode)       ; scratchバッファをtext-modeで開く
  (undo-limit (* 128 1024 1024))        ; Undo履歴 Limit
  (undo-strong-limit (* 192 1024 1024)) ; これを超えると古い履歴から削除
  (undo-outer-limit (* 384 1024 1024))  ; これを超えると履歴全削除
  (backup-directory-alist '(("." . "~/.emacs.d/backups/"))) ; バックアップファイル~保存場所
  (auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-saves/" t))) ; 自動保存ファイル保存場所
  :config
  (define-key key-translation-map (kbd "C-h") (kbd "DEL")) ; C-h -> BS
  (define-key key-translation-map (kbd "C-;") (kbd "C-h")) ; C-; -> ヘルプ
  ;; 行末の不要な空白を強調表示
  (add-hook 'prog-mode-hook (lambda () (setq show-trailing-whitespace t)))
  (add-hook 'text-mode-hook (lambda () (setq show-trailing-whitespace t))))

;; Emacsサーバー起動
(use-package server
  :ensure nil
  :custom
  (server-window 'pop-to-buffer) ; eコマンドで別バッファに表示
  :config
  (unless (server-running-p)
    (server-start)))

;; キーバインド表示
(use-package which-key
  :ensure nil
  :custom
  (which-key-mode t)
  (which-key-idle-delay 0.7)
  (which-key-idle-secondary-delay 0)
  (which-key-sort-order nil) ; 設定順で表示
  )

;; Emacs Lisp強化ヘルプ
(use-package helpful
  :ensure t
  :bind
  (("C-h f" . helpful-callable)
   ("C-h v" . helpful-variable)
   ("C-h k" . helpful-key)
   ("C-h x" . helpful-command)
   ("C-h ." . helpful-at-point)))

;; パスワード管理
(use-package auth-source
  :ensure nil
  :config
  (auth-source-pass-enable))

;; バッファの表示方法についての設定 (display-buffer-alist)
;; 良く使う分割方法
(defconst my/display-split
  '((display-buffer-pop-up-window
     display-buffer-use-some-window)
    (side . right)
    (window-width . 0.5)))

;; 上に書いたものが優先される
(let ((rules
       (list
        ;; flymake-show-project-diagnostics
        `("\\*Flymake diagnostics" ,@my/display-split)
        ;; vterm (multi-vterm: *vterminal<N>*)
        `("\\*vterminal" ,@my/display-split
          (window-parameters . ((dedicated . t))))
        ;; ielm
        `("\\*ielm\\*" ,@my/display-split))))
  (setq display-buffer-alist (append rules display-buffer-alist)))

;; posframe
(use-package posframe
  :ensure t)

(provide '00-basic)
;;; 00-basic.el ends here
