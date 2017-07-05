;;; setup-web.el --- Setup Web -*- lexical-binding: t -*-

(use-package
  js2-mode
  :mode "\\.js\\'"
  :config
  (define-key js2-mode-map (kbd "C-c C-i") 'js2-jump-to-definition))

(use-package
  tern
  :config
  (add-hook 'js2-mode-hook 'tern-mode))

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

(use-package helm-css-scss)

(provide 'setup-web)
