(require 'boot.repl)
(swap! boot.repl/*default-dependencies*
       concat '[[cider/cider-nrepl "0.16.0"]])

(swap! boot.repl/*default-middleware*
       concat '[cider.nrepl/cider-middleware])
