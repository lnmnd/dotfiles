(use-package
  js2-mode
  :mode "\\.js\\'")

(use-package
  skewer-mode
  :config
  (add-hook 'js2-mode-hook 'skewer-mode))

(use-package
  tern
  :config
  (add-hook 'js2-mode-hook 'tern-mode)
  (define-key tern-mode-keymap (kbd "C-c C-r") 'js-send-region))

(use-package tern-auto-complete)

(use-package
  web-mode
  :mode "\\.html?\\'" "\\.css\\'" "\\.djhtml\\'"
  :config
  ;; no tabs
  (setq-default indent-tabs-mode nil)
  ;; 2 spaces
  (setq web-mode-markup-indent-offset 2))

(use-package
  twig-mode
  :mode "\\.twig\\'")

(use-package
  mustache-mode
  :mode "\\.mustache\\'")


(use-package
  scss-mode
  :mode "\\.scss\\'")

(provide 'setup-web)
