{:user
 {:dependencies [[leiningen #=(leiningen.core.main/leiningen-version)]
                 [org.clojure/tools.namespace "0.2.11"]
                 [vvvvalvalval/scope-capture "0.3.2"]]
  :plugins [[jonase/eastwood "0.2.3"] ;; lint
            [lein-kibit "0.1.5"] ;; static code analyzer
]
  :injections [(require 'sc.api)]}

 :repl
 {:plugins [[cider/cider-nrepl "0.18.0"]]}}
