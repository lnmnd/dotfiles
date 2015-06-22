;; factor
(use-package
  factor-mode
  :load-path "~/src/factor/misc/fuel"
  :config
  (use-package
    fuel-mode
    :init
    (setq fuel-factor-root-dir "~/src/factor")))
