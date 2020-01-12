;;; setup-web-mode.el --- Setup web-mode -*- lexical-binding: t -*-

(require 'yasnippet)

;; no tabs
(setq-default indent-tabs-mode nil)
;; 2 spaces
(setq web-mode-markup-indent-offset 2)
;; Disable auto-pairing
(setq web-mode-enable-auto-pairing nil)
(add-hook 'web-mode-hook #'enable-show-trailing-whitespace)
(defun setup-web-mode-hook ()
  (yas-reload-all)
  (yas-minor-mode))
(add-hook 'web-mode-hook #'setup-web-mode-hook)

(provide 'setup-web-mode)
