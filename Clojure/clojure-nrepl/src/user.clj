(ns user
  (:require [nrepl.server :as nrepl]
            [cider.nrepl :as cider]))

(defn start-nrepl
  "Starts an nREPL server with CIDER middleware."
  [_]
  (let [port 7888
        bind "127.0.0.1"]
    (println (format "Starting nREPL server for Calva on port %d at %s" port bind))
    (nrepl/start-server
     :port port
     :bind bind
     :handler cider/cider-nrepl-handler)

    ;; Prevent the process from exiting immediately so it can run as a systemd service
    (println "nREPL server is running. Press Ctrl-C to stop.")
    @(promise)))