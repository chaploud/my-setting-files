;;; 03-evil.el --- Vimエミュレーション -*- lexical-binding: t; -*-

;;; Commentary:
;;; Code:

;; Undoシステム
(use-package undo-fu
  :ensure t)

;; セッションまたぎのUndo
(use-package undo-fu-session
  :ensure t
  :after undo-fu
  :custom
  (undo-fu-session-global-mode t))

;; Undoツリー
(use-package vundo
  :ensure t
  :after undo-fu)

;; evil本体
(use-package evil
  :ensure t
  :after (undo-fu undo-fu-session)
  :custom
  (evil-want-integration t)
  (evil-want-keybinding nil)
  (evil-want-C-u-scroll t)
  (evil-undo-system 'undo-fu)
  (evil-symbol-word-search t) ; シンボル単位での検索
  (evil-shift-width 2)
  (evil-mode t)
  (evil-kill-on-visual-paste nil)
  :bind
  ;; Emacsキーバインドも一部使う
  (:map evil-insert-state-map
        ("C-f" . nil)
        ("C-b" . nil)
        ("C-n" . nil)
        ("C-p" . nil)
        ("C-a" . nil)
        ("C-e" . nil)
        ("C-d" . nil)
        ("C-k" . nil)
        ("C-y" . nil)
        ("C-S-h" . #'backward-kill-sexp))
  (:map evil-normal-state-map
        ("C-." . nil))
  (:map evil-motion-state-map
        ("," . nil))
  :config
  ;; 折り返しがある行でgj/gkが面倒なので
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)
  ;; 最初からnormalモードであってほしいバッファ
  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

;; 代表的なモードに対するevilキーバインドを提供
(use-package evil-collection
  :ensure t
  :after evil
  :custom
  (evil-collection-setup-minibuffer t)
  (evil-collection-setup-debugger-keys t)
  (evil-collection-key-blacklist '("C-j" "C-k" "SPC"))
  (evil-collection-magit-want-horizontal-movement t)
  :config
  (evil-collection-init))

;; fdでESC
(use-package evil-escape
  :ensure t
  :after evil
  :custom
  (evil-escape-mode t)
  :config
  ;; isearch検索入力中にfキーの反応が遅れるのを防止
  (add-to-list 'evil-escape-inhibit-functions
               (lambda () isearch-mode)))

;; 囲み系の操作
(use-package evil-surround
  :ensure t
  :after evil
  :custom
  (global-evil-surround-mode t))

;; 検索ヒット数表示
(use-package evil-anzu
  :ensure t
  :after evil
  :custom
  (global-anzu-mode t))

;; 編集操作をハイライト
(use-package evil-goggles
  :ensure t
  :after evil
  :custom
  (evil-goggles-mode t)
  ;; 削除系はハイライトせずとも分かる & 反映が遅れるとストレスなのでオフ
  (evil-goggles-enable-delete nil)
  (evil-goggles-enable-change nil))

;; コメントアウト
(use-package evil-commentary
  :ensure t
  :config
  (evil-commentary-mode t))

(provide '03-evil)
;;; 03-evil.el ends here
