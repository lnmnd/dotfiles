;; tern
(add-to-list 'load-path "~/.emacs.d/lib/tern")
(autoload 'tern-mode "~/.emacs.d/lib/tern/emacs/tern.el" nil t)

(use-package
  js2-mode
  :ensure t
  :mode "\\.js\\'"
  :config
  (setq inferior-js-program-command "phantomjs")
  (add-hook 'js2-mode-hook (lambda () (tern-mode t))))

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

