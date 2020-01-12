;;; setup-css-mode.el --- Setup css-mode -*- lexical-binding: t -*-

(require 'counsel-css)
(require 'skewer-css)

(define-key css-mode-map (kbd "C-c C-h") 'counsel-css)
(defun setup-css-mode-hook ()
  (enable-show-trailing-whitespace)
  (skewer-css-mode))
(add-hook 'css-mode-hook #'setup-css-mode-hook)

(provide 'setup-css-mode)
