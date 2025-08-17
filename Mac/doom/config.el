;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
;; (setq user-full-name "John Doe"
;;       user-mail-address "john@doe.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-symbol-font' -- for symbols
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
(setq doom-font (font-spec :family "Fira Code" :size 15 :weight 'normal)
      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 15))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'spacemacs-dark)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")


;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

(custom-set-faces!
  '(bold ((t (:weight semi-bold)))))
(custom-set-variables
 '(which-key-idle-delay 0.2)
 '(which-key-idle-secondary-delay 0)
 '(which-key-use-C-h-commands t)
 )

(setq fancy-splash-image "~/Documents/my-setting-files/Mac/doom/doomEmacsTokyoNight.svg")

;; === Spacemacs風キーバインド
(setq-default evil-escape-key-sequence "fd")
(setq doom-localleader-key ",")
(define-key key-translation-map (kbd "C-h") (kbd "<DEL>"))
(define-key evil-insert-state-map (kbd "C-d") #'delete-char)
(define-key evil-insert-state-map (kbd "C-k") #'kill-line)
(map! :after corfu
      :map corfu-mode-map
      :i "C-p" #'previous-line
      :i "C-n" #'next-line)
;; コメントイン/アウト
(map! "s-;" #'evilnc-comment-or-uncomment-lines) ; M-;

;; corfu(補完)
(after! corfu
  (setq corfu-preselect 'first))
(after! corfu-popupinfo
  (setq corfu-popupinfo-delay '(0.2 . 0.2)))
(after! orderless
  (setq orderless-matching-styles '(orderless-literal
                                    orderless-regexp
                                    orderless-flex))
  (setq completion-styles '(orderless partial-completion basic))
  (setq completion-category-defaults nil)
  (setq completion-category-overrides '((file (styles partial-completion))))
  )

;; === quit時にy/nを聞かない
(setq confirm-kill-emacs nil)

;; === ddskk
(defun switch-ime (input-source)
  (call-process "macism" nil 0 nil input-source))
(add-function :after after-focus-change-function
              (lambda ()
                (if (frame-focus-state)
                    (switch-ime "com.apple.keylayout.ABC")
                  (switch-ime "net.mtgto.inputmethod.macSKK.hiragana"))))

(use-package! skk
  :config
  (setq skk-server-host "127.0.0.1")
  (setq skk-server-portnum 1178)
  (setq skk-dcomp-activate t)
  (setq skk-egg-like-newline t)
  (setq skk-delete-implies-kakutei nil)
  (setq skk-use-color-cursor nil)
  (setq skk-show-candidates-nth-henkan-char 3)
  (remove-hook 'doom-escape-hook #'skk-mode-exit)
  :hook
  (evil-normal-state-entry-hook
   . (lambda ()
       (when (bound-and-true-p skk-mode)
         (skk-latin-mode-on))))
  )

(defun turn-on-skk ()
  (skk-mode)
  (skk-latin-mode-on))

(add-hook 'text-mode-hook #'turn-on-skk)
(add-hook 'prog-mode-hook #'turn-on-skk)

;; ddskkとDoom Emacsの相性を解消
(map! :i "C-j" nil)
(map! :i "C-g" nil)
(map! :after evil-org
      :map evil-org-mode-map
      :i "C-j" (cmds! (org-at-table-p #'org-table-next-row nil)))

;; === vtermのトグル
(map! "C-'" #'+vterm/toggle)

;; === 括弧の色づけ
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

;; === markdown
(map! :map markdown-mode-map
      :n ",," #'markdown-toggle-gfm-checkbox
      :i "C-d" #'delete-char)
