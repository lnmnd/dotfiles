{:user
 {:dependencies [[leiningen #=(leiningen.core.main/leiningen-version)]
                 [org.clojure/tools.namespace "0.2.11"]
                 [im.chit/vinyasa "0.4.7"]]
  :plugins [[lein-cljfmt "0.5.3"] ;; formatting
            [jonase/eastwood "0.2.3"] ;; lint
            [lein-kibit "0.1.2"] ;; static code analyzer
            [codox "0.8.13"]]
  :injections [(require '[vinyasa.inject :as inject])
               (inject/in [clojure.tools.namespace.repl :refer [refresh]]
                          [vinyasa.inject :refer [inject]]
                          [vinyasa.maven pull])]}
 
 :repl
 {:plugins [[cider/cider-nrepl "0.12.0"]
            [refactor-nrepl "2.0.0-SNAPSHOT"]]}}
