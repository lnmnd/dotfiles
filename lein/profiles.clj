{:user
 {:dependencies [[leiningen #=(leiningen.core.main/leiningen-version)]
                 [org.clojure/tools.namespace "0.2.11"]
                 [im.chit/vinyasa "0.4.7"]]
  :plugins [[jonase/eastwood "0.2.3"] ;; lint
            [lein-kibit "0.1.5"] ;; static code analyzer
]
  :injections [(require '[vinyasa.inject :as inject])
               (inject/in [clojure.tools.namespace.repl :refer [refresh]]
                          [vinyasa.inject :refer [inject]]
                          [vinyasa.maven pull])]}

 :repl
 {:plugins [[cider/cider-nrepl "0.18.0"]]}}
