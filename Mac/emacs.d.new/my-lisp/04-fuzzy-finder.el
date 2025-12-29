;;; 04-fuzzy-finder.el --- 補完・検索システム -*- lexical-binding: t; -*-

;;; Commentary:
;; ミニバッファ補完 (vertico) とバッファ内補完 (corfu)

;;; Code:

;; 再帰的ミニバッファを許可
(setq enable-recursive-minibuffers t)

;; 垂直補完UI
(use-package vertico
  :ensure t
  :custom
  (vertico-mode t)
  (vertico-cycle t)
  (vertico-count 15)
  (vertico-resize nil))

;; 柔軟なマッチング
(use-package orderless
  :ensure t
  :custom
  (completion-styles '(basic partial-completion orderless))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles basic))
                                   (corfu (styles basic partial-completion orderless))))
  ;; 正規表現とあいまい検索もデフォルトで有効に
  ;; 完全一致(literal)を優先させたいときは先頭に`='をつける
  (orderless-matching-styles '(orderless-literal
                               orderless-flex
                               orderless-regexp)))

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
  ;; もとのconsult-ripgre-pargsの値に追記したほうがいい
  (let ((additional-args '("--hidden"
                           "--glob=!.git/**"
                           "--glob=!node_modules/**"
                           "--glob=!target/**"
                           "--glob=!*.lock"
                           "--glob=!*.log")))
    (setq consult-ripgrep-args
          (concat consult-ripgrep-args " " (string-join additional-args " "))))


  ;; 現在バッファのディレクトリ以下でripgrep検索する関数
  (defun my-consult-ripgrep-current-dir ()
    "Search in the current buffer's directory (or `default-directory') using consult-ripgrep."
    (interactive)
    (let ((dir (or (and (buffer-file-name)
                        (file-name-directory (buffer-file-name)))
                   default-directory)))
      (consult-ripgrep dir))))

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

;; バッファ内補完UI
(use-package corfu
  :ensure t
  :custom
  (global-corfu-mode t)
  (corfu-popupinfo-mode t)
  (corfu-history-mode t)
  (corfu-auto t)
  (corfu-auto-delay 0)
  (corfu-popupinfo-delay 0)
  (corfu-auto-prefix 1)
  (corfu-cycle t)
  (corfu-preselect 'prompt)
  (corfu-quit-no-match 'separator)
  (corfu-on-exact-match nil)
  (tab-always-indent 'complete))

;; 補完ポップアップのアイコン
(use-package nerd-icons-corfu
  :ensure t
  :after (corfu nerd-icons)
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(provide '04-fuzzy-finder)

;;; 04-fuzzy-finder.el ends here
