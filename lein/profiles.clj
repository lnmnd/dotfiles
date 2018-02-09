{:user
 {:dependencies [[leiningen #=(leiningen.core.main/leiningen-version)]
                 [org.clojure/tools.namespace "0.2.11"]
                 [im.chit/vinyasa "0.4.7"]
                 [com.jakemccrary/lein-test-refresh "0.16.0"]
                 [pjstadig/humane-test-output "0.8.0"]]
  :plugins [[lein-cljfmt "0.5.3"] ;; formatting
            [jonase/eastwood "0.2.3"] ;; lint
            [lein-kibit "0.1.5"] ;; static code analyzer
            [codox "0.8.13"]
            [com.jakemccrary/lein-test-refresh "0.16.0"]]
  :injections [(require '[vinyasa.inject :as inject])
               (inject/in [clojure.tools.namespace.repl :refer [refresh]]
                          [vinyasa.inject :refer [inject]]
                          [vinyasa.maven pull])

               (require 'pjstadig.humane-test-output)
               (pjstadig.humane-test-output/activate!)]

  :test-refresh {:notify-command ["zenity" "--error" "--text"]
                 :notify-on-success false}}
 
 :repl
 {:plugins [[cider/cider-nrepl "0.16.0"]]}}
