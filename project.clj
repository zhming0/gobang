(defproject gobang-AI "0.0.0"
  :plugins [[lein-cljsbuild "1.0.2"]]
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [org.clojure/clojurescript "0.0-2156"]]
  :source-paths ["cljs-src"]
  :cljsbuild {
    :builds [{
      :source-paths ["cljs-src"]
      :compiler {
        :warning true
        :output-to "gobang-ai.js"  
        :optimizations :whitespace
        :pretty-print true}}]})
