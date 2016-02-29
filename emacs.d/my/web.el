(use-package
  js2-mode
  :ensure t
  :mode "\\.js\\'")

(use-package
  js-comint
  :ensure t
  :mode "\\.js\\'"
  :config
  (setq inferior-js-program-command "node")
  (setq inferior-js-program-arguments '("--interactive"))
  (setenv "NODE_NO_READLINE" "1"))

(use-package
  tern
  :ensure t
  :config
  (add-hook 'js2-mode-hook 'tern-mode)
  (define-key tern-mode-keymap (kbd "C-c C-r") 'js-send-region))

(use-package
  tern-auto-complete
  :ensure t)

(use-package
  web-mode
  :ensure t
  :mode "\\.html?\\'"
  :config
  ;; no tabs
  (setq-default indent-tabs-mode nil)
  ;; 2 spaces
  (setq web-mode-markup-indent-offset 2))

(use-package
  twig-mode
  :ensure t
  :mode "\\.twig\\'")

(use-package
  mustache-mode
  :ensure t
  :mode "\\.mustache\\'")

