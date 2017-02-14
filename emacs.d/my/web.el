(use-package
  js2-mode
  :ensure t
  :mode "\\.js\\'")

(use-package
  skewer-mode
  :ensure t
  :config
  (add-hook 'js2-mode-hook 'skewer-mode))

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
  :mode "\\.html?\\'" "\\.css\\'" "\\.djhtml\\'"
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


(use-package
  scss-mode
  :ensure t
  :mode "\\.scss\\'")
