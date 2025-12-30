;;; 08-fuzzy-finder.el --- 検索システム -*- lexical-binding: t; -*-

;;; Commentary:
;;; Code:

;; 垂直補完UI
(use-package vertico
  :ensure t
  :custom
  (vertico-mode t)
  (vertico-cycle t)
  (vertico-count 15)
  (vertico-resize nil))

;; 柔軟なマッチング
;; デフォルトは literal と regexp のみ (flex は緩すぎるので無効)
;; flex を使いたい場合は検索パターンの末尾に ~ をつける (例: oo~)
(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles basic partial-completion))))
  (orderless-matching-styles '(orderless-literal orderless-regexp))
  :config
  ;; ~ で終わるパターンのみ flex を有効化
  (defun my-orderless-flex-dispatcher (pattern _index _total)
    "Enable flex matching when PATTERN ends with ~."
    (when (string-suffix-p "~" pattern)
      `(orderless-flex . ,(substring pattern 0 -1))))
  (setq orderless-style-dispatchers '(my-orderless-flex-dispatcher)))

;; 補完候補に注釈
(use-package marginalia
  :ensure t
  :after vertico
  :custom
  (marginalia-mode t))

;; 便利な検索コマンド集
(use-package consult
  :ensure t
  :custom
  (consult-async-min-input 2)
  (consult-narrow-key "<") ; consultのミニバッファで< fなどで絞り込める
  :init
  (setq xref-show-xrefs-function #'consult-xref)
  (setq xref-show-definitions-function #'consult-xref)
  :hook
  (completion-list-mode . consult-preview-at-point-mode)
  :config
  ;; もとのconsult-ripgrep-argsの値に追記
  (let ((additional-args '("--hidden"
                           "--glob=!.git/**"
                           "--glob=!node_modules/**"
                           "--glob=!target/**"
                           "--glob=!*.lock"
                           "--glob=!*.log")))
    (setq consult-ripgrep-args
          (concat consult-ripgrep-args " " (string-join additional-args " ")))))

;; 現在バッファのディレクトリ以下でripgrep検索
(defun my-consult-ripgrep-current-dir ()
  "Search in the current buffer's directory using consult-ripgrep."
  (interactive)
  (let ((dir (or (and (buffer-file-name)
                      (file-name-directory (buffer-file-name)))
                 default-directory)))
    (consult-ripgrep dir)))

;; 補完候補へのアクション
(use-package embark
  :ensure t
  :bind
  (:map minibuffer-local-map
        ("C-." . embark-act)
        ("C-," . embark-export)))

;; embarkをconsultから使う
(use-package embark-consult
  :ensure t
  :after (embark consult)
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;;===== 一括置換操作 ====================================================
;; 1. `SPC s s' (consult-line)や `SPC s p' (consult-ripgrep)で候補表示
;; 2. `C-,' (embark-export)でembark-collect-modeに
;; 3. OccurやWgrepの違いはあるが, `i'で編集モードに入る
;; 4. `:%s;xxx;yyy;g' などで一括置換 (普通に編集してもいい)
;; 5. `ESC'で編集モードを抜ける (この際に変更を保存するか聞かれることもある)
;;====================================================================
(use-package wgrep
  :ensure t
  :commands (wgrep-change-to-wgrep-mode)
  :custom
  (wgrep-auto-save-buffer t)
  (wgrep-change-readonly-file t))

(provide '08-fuzzy-finder)
;;; 08-fuzzy-finder.el ends here
