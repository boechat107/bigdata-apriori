(ns bigdata-apriori.core
  (:use 
    [clojure.tools.cli :only [cli]])
  (:require 
    [clojure.io :as io]
    [bigdata-apriori.apriori :as alg])
  (:gen-class)
  )

(defn -main
  [& args]
  (let [[opts _ banner] 
        (cli args 
             ["-h" "--help" "Show help" :flag true :default false]
             ["-f" "--file" "Path of input file"]
             ["-s" "--support" 
              "Support value (double number) to select the most frequent elements"
              :default 0.1])
        fp (:file opts)]
    (cond 
      (:help opts) (println banner)
      fp (with-open [rdr (io/reader fp)]
           ;; TODO: file parsing
           (alg/apriori (:support opts) (line-seq rdr)))
      )
    )
  )
