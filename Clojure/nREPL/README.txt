~/.config/systemd/user/clojure-nrepl.servieに配置
systemctl --user daemon-reload
systemctl --user enable clojure-nrepl.service
systemctl --user start clojure-nrepl.service
systemctl --user status clojure-nrepl.service
journalctl --user -u clojure-nrepl.service

nREPL Portは.nrepl-portがあれば見る
7888