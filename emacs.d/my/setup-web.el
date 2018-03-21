;;; setup-web.el --- Setup Web -*- lexical-binding: t -*-

(use-package
  js2-mode
  :mode "\\.js\\'"
  :config
  (define-key js2-mode-map (kbd "C-c C-i") 'js2-jump-to-definition))

(use-package
  js2-refactor
  :config
  (add-hook 'js2-mode-hook 'js2-refactor-mode)
  (define-key js2-mode-map (kbd "C-k") 'js2r-kill))

(use-package
  tern
  :config
  (add-hook 'js2-mode-hook 'tern-mode))

(use-package tern-auto-complete)

(use-package
  web-mode
  :mode "\\.html?\\'" "\\.djhtml\\'"
  :config
  ;; no tabs
  (setq-default indent-tabs-mode nil)
  ;; 2 spaces
  (setq web-mode-markup-indent-offset 2)
  ;; Disable auto-pairing
  (setq web-mode-enable-auto-pairing nil))

(use-package
  twig-mode
  :mode "\\.twig\\'")

(use-package
  mustache-mode
  :mode "\\.mustache\\'")

(use-package
  css-mode
  :mode "\\.css\\'"
  :config
  (define-key css-mode-map (kbd "C-c C-o") 'helm-css-scss))

(use-package
  scss-mode
  :mode "\\.scss\\'"
  :config
  (setq scss-compile-at-save nil))

(use-package helm-css-scss)

(use-package
  skewer-mode
  :config
  (add-hook 'js2-mode-hook 'skewer-mode)
  (add-hook 'css-mode-hook 'skewer-css-mode))

(provide 'setup-web)
