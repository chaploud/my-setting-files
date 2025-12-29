;;; 03-evil.el --- Vimエミュレーション -*- lexical-binding: t; -*-

;;; Commentary:
;; evil と関連パッケージ

;;; Code:

;; undo-fu - undoシステム
(use-package undo-fu :ensure t)

(use-package undo-fu-session
  :ensure t
  :config (undo-fu-session-global-mode t))

;; evil本体
(use-package evil
  :ensure t
  :custom
  (evil-want-integration t)
  (evil-want-keybinding nil)  ; evil-collection用
  (evil-want-C-u-scroll t)
  (evil-undo-system 'undo-fu)
  (evil-symbol-word-search t)
  (evil-shift-width 2)
  (evil-kill-on-visual-paste nil)
  :config
  (evil-mode t)
  ;; j/kで表示行移動
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)
  ;; 初期状態の設定
  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

;; evil-collection - 各モード用キーバインド
(use-package evil-collection
  :ensure t
  :after evil
  :custom
  (evil-collection-setup-minibuffer t)
  (evil-collection-key-blacklist '("C-j" "C-k" "SPC"))
  :config (evil-collection-init))

;; evil-escape - fdでESC
(use-package evil-escape
  :ensure t
  :after evil
  :config
  (evil-escape-mode t)
  (add-to-list 'evil-escape-inhibit-functions (lambda () isearch-mode)))

;; evil-surround - 囲み操作
(use-package evil-surround
  :ensure t
  :config (global-evil-surround-mode t))

;; evil-anzu - 検索ヒット数表示
(use-package evil-anzu
  :ensure t
  :config (global-anzu-mode t))

;; evil-goggles - 編集操作をハイライト
(use-package evil-goggles
  :ensure t
  :custom
  (evil-goggles-enable-delete nil)
  (evil-goggles-enable-change nil)
  :config (evil-goggles-mode t))

;; evil-commentary - コメントアウト (gc)
(use-package evil-commentary
  :ensure t
  :config (evil-commentary-mode t))

;; evil-numbers - 数値インクリメント
(use-package evil-numbers
  :ensure t
  :bind (:map evil-normal-state-map
              ("C-a" . evil-numbers/inc-at-pt)))

(provide '03-evil)
;;; 03-evil.el ends here
