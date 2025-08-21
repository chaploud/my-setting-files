# Configuration for Clojure nREPL registered with systemd

## How to register the nREPL service

- Place this parent directory itself in `~/.config/systemd/user/nrepl/` or create a symbolic link to it

```bash
cd ~/.config/systemd/user
systemctl --user daemon-reload
systemctl --user enable nrepl/clojure-nrepl.service # auto start on login
systemctl --user start clojure-nrepl.service # start the service (for this login session)
systemctl --user status clojure-nrepl.service # check the status
```

After first registration, you can also use the following commands to manage the service:

```bash
systemctl --user disable clojure-nrepl.service # auto start on login
systemctl --user restart clojure-nrepl.service # NOTE: for restart, you need to stop it first
```
