;;; 05-tools.el --- ツール群 -*- lexical-binding: t; -*-

;;; Commentary:
;;; Code:

;; ブックマーク
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

;; ファイルのリネーム・削除
(use-package bufferfile
  :ensure t)

;; eldocのポップアップ表示
(use-package eldoc-box
  :ensure t)

;; ワークスペース
(use-package perspective
  :ensure t
  :custom
  (persp-state-default-file "~/.emacs.d/workspaces/default")
  (persp-suppress-no-prefix-key-warning t)
  (persp-sort 'created)
  (persp-modestring-short t)
  (persp-mode t))

;; ファイルツリー
(use-package treemacs
  :ensure t
  :defer t
  :custom
  (treemacs-follow-mode t)
  (treemacs-project-follow-cleanup t))

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

(use-package treemacs-nerd-icons
  :ensure t
  :after (treemacs nerd-icons)
  :config
  (treemacs-nerd-icons-config))

(use-package treemacs-perspective
  :ensure t
  :after (treemacs perspective)
  :config
  (treemacs-set-scope-type 'Perspectives))

;; HTTPクライアント
(use-package restclient
  :ensure t
  :mode (("\\.http\\'" . restclient-mode)
         ("\\.rest\\'" . restclient-mode))
  :custom (restclient-same-buffer-response t))

;; Dockerコンテナ管理
(use-package docker
  :ensure t
  :defer t
  :custom
  (docker-container-columns
   '((:name "Names" :width 30 :template "{{ json .Names }}" :sort nil :format nil)
     (:name "Status" :width 30 :template "{{ json .Status }}" :sort nil :format nil)
     (:name "Ports" :width 30 :template "{{ json .Ports }}" :sort nil :format nil)))
  (docker-container-default-sort-key '("Names")))

;; RSSリーダー
(use-package elfeed
  :ensure t
  :defer t
  :custom
  (elfeed-search-title-min-width 40)
  (elfeed-search-title-max-width 120)
  (elfeed-search-filter "@1-week-ago +unread")
  (elfeed-search-print-entry-function #'my-elfeed-search-print-entry)
  (shr-use-fonts nil)
  (elfeed-feeds
   '(
     ;; ===== Financial / Algo Trading / ML =====
     "https://www.quantstart.com/feed/"
     "https://blog.quantinsti.com/rss"
     "https://quantpedia.com/blog/feed/"
     "https://feeds.feedburner.com/Quantocracy"

     ;; ===== Programming Languages / Language Implementation =====
     "https://langdev.org/rss.xml"
     "https://llvm.org/blog/rss.xml"
     "https://feeds.feedburner.com/plweekly"

     ;; ===== Software Architecture =====
     "https://developertoarchitect.com/lessons/index.xml"

     ;; ===== Clojure Ecosystem =====
     "https://clojure.org/news/deref.rss"
     "https://planet.clojure.in/atom.xml"
     "https://www.lambda.is/blog/rss"
     "https://purelyfunctional.tv/blog/rss"
     "https://blog.juxt.pro/rss.xml"

     ;; ===== Emacs / Vim =====
     "https://planet.emacslife.com/atom.xml"

     ;; ===== WebAssembly Core =====
     "https://webassembly.org/feed.xml"
     "https://www.w3.org/blog/webassembly/feed/"
     "https://github.com/WebAssembly/spec/releases.atom"

     ;; ===== Wasmtime / Wasmer / Bytecode Alliance =====
     "https://github.com/bytecodealliance/wasmtime/releases.atom"
     "https://bytecodealliance.org/articles/index.xml"
     "https://github.com/wasmerio/wasmer/releases.atom"
     "https://wasmer.io/feed.xml"

     ;; ===== WASM Tooling / Runtime / Ecosystem =====
     "https://github.com/WebAssembly/wasi/releases.atom"
     "https://github.com/WebAssembly/component-model/releases.atom"
     "https://github.com/fermyon/spin/releases.atom"

     ;; ===== General Tech / Global =====
     "https://dev.to/feed/"
     "https://speakerdeck.com/c/programming.atom"
     "https://speakerdeck.com/c/technology.atom"

     ;; ===== General Tech / Japanese Sources =====
     "https://zenn.dev/feed"
     "https://qiita.com/popular-items/feed"
     "https://qiita.com/tags/webassembly/feed"
     "https://qiita.com/tags/TypeScript/feed"
     "https://qiita.com/tags/Clojure/feed.atom"
     "https://qiita.com/tags/Emacs/feed.atom"
     "https://qiita.com/tags/Vim/feed.atom"
     "https://rss.itmedia.co.jp/rss/2.0/techtarget.xml"
     "https://codezine.jp/rss/new/20/index.xml"
     "https://realtime.jser.info/feed.xml"
     "https://www.publickey1.jp/atom.xml"
     ))

  :config
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
      (insert (propertize title-column 'face title-faces 'kbd-help title) " "))))

(provide '05-tools)
;;; 05-tools.el ends here
