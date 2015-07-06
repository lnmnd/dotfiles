(use-package
  js2-mode
  :ensure t
  :mode "\\.js\\'"
  :config
  (setq inferior-js-program-command "phantomjs")
  (add-hook 'js2-mode-hook (lambda () (tern-mode t))))

(use-package
  tern
  :ensure t)

(use-package
  moz
  :ensure t
  :config
  (add-hook 'js2-mode-hook 'moz-minor-mode))

(use-package
  web-mode
  :ensure t
  :mode ("\\.html?\\'" "\\.twig\\'")
  :config
  ;; no tabs
  (setq-default indent-tabs-mode nil)
  ;; 2 spaces
  (setq web-mode-markup-indent-offset 2))

(use-package
  mustache
  :ensure t
  :mode "\\.mustache\\'")

