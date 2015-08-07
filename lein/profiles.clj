{:user
 {:dependencies [[leiningen #=(leiningen.core.main/leiningen-version)]
                 [org.clojure/tools.namespace "0.2.11"]
                 [im.chit/vinyasa "0.3.4"]
                 [alembic "0.3.2"]]
  :plugins [[cider/cider-nrepl "0.9.1"]
            [lein-typed "0.3.5"]
            [codox "0.8.13"]
            [lein-marginalia "0.8.0"]]
  :injections [(require '[vinyasa.inject :as inject])
               (inject/in clojure.core
                          [clojure.tools.namespace.repl :refer [refresh]]
                          [alembic.still :refer [[distill pull] lein]])]}}
