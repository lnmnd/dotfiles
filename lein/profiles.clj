{:user
 {:dependencies [[leiningen #=(leiningen.core.main/leiningen-version)]
                 [org.clojure/tools.namespace "0.2.11"]
                 [im.chit/vinyasa "0.3.4"]
                 [alembic "0.3.2"]]
  :plugins [[lein-cljfmt "0.3.0"] ;; formatting
            [jonase/eastwood "0.2.1"] ;; lint
            [lein-kibit "0.1.2"] ;; static code analyzer
            [lein-typed "0.3.5"]
            [codox "0.8.13"]
            [lein-marginalia "0.8.0"]]
  :injections [(require '[vinyasa.inject :as inject])
               (inject/in [clojure.tools.namespace.repl :refer [refresh]]
                          [alembic.still :refer [[distill pull] lein]])]}
 
 :repl
 {:plugins [[cider/cider-nrepl "0.10.0"]
            [refactor-nrepl "2.0.0-SNAPSHOT"]]}}
