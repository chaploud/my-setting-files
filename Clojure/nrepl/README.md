# Configuration for Clojure nREPL registered with systemd

## How to register the nREPL service

- Place this parent directory itself in `~/.config/systemd/user/clojure-nrepl/` or create a symbolic link to it
- `clojure-nrepl.service` will be called, dependencies will be resolved according to `deps.edn`, and nREPL will start with `src/user.clj`

```sh
systemctl --user daemon-reload # Reload the service files
systemctl --user enable clojure-nrepl/clojure-nrepl.service # Enable the service to start automatically
systemctl --user start clojure-nrepl.service # (For this login session) Start the service
systemctl --user status clojure-nrepl.service # Check the status
```

## Connecting from Calva

- If you configure as in `vscode/settings.json`, you can automatically execute the connect to running REPL command
- If you start with `.nrepl-port` file set to 7888, the port selection will also be automatic
