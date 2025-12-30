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
  :init
  (setq persp-suppress-no-prefix-key-warning t)
  (persp-mode)
  :custom
  (persp-sort 'created)
  (persp-modestring-short t))

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
  :custom
  (docker-container-columns
   '((:name "Names" :width 30 :template "{{ json .Names }}" :sort nil :format nil)
     (:name "Status" :width 30 :template "{{ json .Status }}" :sort nil :format nil)
     (:name "Ports" :width 30 :template "{{ json .Ports }}" :sort nil :format nil)))
  (docker-container-default-sort-key '("Names")))

;; RSSリーダー
(use-package elfeed
  :ensure t
  :custom
  (elfeed-search-title-min-width 40)
  (elfeed-search-title-max-width 120)
  (elfeed-search-filter "@1-week-ago +unread")
  (elfeed-search-print-entry-function #'my-elfeed-search-print-entry)
  (shr-use-fonts nil)
  (elfeed-feeds
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
