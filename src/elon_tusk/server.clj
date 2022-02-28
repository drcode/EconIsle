(ns elon-tusk.server
  (:require [clojure.edn :as edn]
            [qlkit.core :as ql]
            [elon-tusk.parsers :as pa]))

(defn handler [req]
  (when (= (:uri req) "/endpoint")
    (let [query  (edn/read-string (slurp (:body req)))
          result (ql/parse-query query)]
      {:status 200
       :body   (pr-str result)})))

(ql/mount {:parsers {:read   pa/read
                     :mutate pa/mutate}})


