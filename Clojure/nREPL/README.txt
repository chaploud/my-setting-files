~/.config/systemd/user/clojure-nrepl.servieに配置
systemctl --user daemon-reload # serviceファイルを読み込む
systemctl --user enable clojure-nrepl.service # 自動起動するように設定
systemctl --user start clojure-nrepl.service # (今回ログイン時用)起動する
systemctl --user status clojure-nrepl.service # 状態を確認する

nREPL Portは.nrepl-portがあれば見る
7888