(use-package
  js2-mode
  :ensure t
  :mode "\\.js\\'")

(use-package
  moz
  :ensure t
  :config
  (add-hook 'js2-mode-hook 'moz-minor-mode))

(use-package
  tern
  :ensure t
  :config
  (add-hook 'js2-mode-hook 'tern-mode)
  (define-key tern-mode-keymap (kbd "C-c C-r") 'moz-send-region))

(use-package
  tern-auto-complete
  :ensure t)

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

