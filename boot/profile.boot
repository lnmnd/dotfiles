(require 'boot.repl)
(swap! boot.repl/*default-dependencies*
       concat '[[cider/cider-nrepl "0.15.1"]])

(swap! boot.repl/*default-middleware*
       concat '[cider.nrepl/cider-middleware])
