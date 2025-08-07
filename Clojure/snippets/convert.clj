#!/usr/bin/env bb

;; bb convert.clj shadow-css-snippets.edn clojure_snippet.code-snippets
;; その後エディタでJSONフォーマット

(require '[clojure.edn :as edn])

(let [[src-edn dest-json] *command-line-args*
      src (-> src-edn
              slurp
              edn/read-string
              :additional-snippets)
      dest (->> src
                (map (fn [{:keys [name detail snippet]}]
                       {(str "shadow-css/" name)
                        {:scope "clojure"
                         :prefix name
                         :body [snippet]
                         :description detail}}))
                (into {})
                (json/generate-string))]
  (spit dest-json dest))