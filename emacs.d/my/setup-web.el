;;; setup-web.el --- Setup Web -*- lexical-binding: t -*-

(defun my-js-format-code ()
  (shell-command-to-string (format "eslint --fix %s" buffer-file-name))
  (revert-buffer :ignore-auto :noconfirm))

(defun js-load-sc ()
  (interactive)
  (find-file (expand-file-name "~/.emacs.d/my/js-scope-capture.txt"))
  (skewer-load-buffer)
  (kill-buffer))

(use-package
  js2-mode
  :mode "\\.js\\'"
  :config
  (define-key js2-mode-map (kbd "C-c C-h") #'counsel-semantic-or-imenu)
  (define-key js2-mode-map (kbd "C-c C-i") 'js2-jump-to-definition)
  (define-key js2-mode-map (kbd "C-c C-o") 'xref-pop-marker-stack)
  (add-hook 'js2-mode-hook #'enable-show-trailing-whitespace)
  (add-hook 'js2-mode-hook #'flycheck-mode)
  (add-hook 'js2-mode-hook (lambda ()
			     (setq indent-tabs-mode nil)
                             (setq js2-basic-offset 2)
			     (add-hook 'after-save-hook #'my-js-format-code nil t))))

(use-package
  tern
  :config
  (add-hook 'js2-mode-hook 'tern-mode))

(use-package
  web-mode
  :mode "\\.html?\\'" "\\.djhtml\\'" "\\.twig\\'"
  :config
  ;; no tabs
  (setq-default indent-tabs-mode nil)
  ;; 2 spaces
  (setq web-mode-markup-indent-offset 2)
  ;; Disable auto-pairing
  (setq web-mode-enable-auto-pairing nil)
  (add-hook 'web-mode-hook #'enable-show-trailing-whitespace))

(use-package
  counsel-css
  :after (ivy))

(use-package
  css-mode
  :after (counsel-css)
  :mode "\\.css\\'"
  :config
  (define-key css-mode-map (kbd "C-c C-h") 'counsel-css)
  (add-hook 'css-mode-hook #'enable-show-trailing-whitespace))

(use-package
  scss-mode
  :mode "\\.scss\\'"
  :config
  (setq scss-compile-at-save nil))

(use-package
  skewer-mode
  :config
  (add-hook 'js2-mode-hook 'skewer-mode)
  (add-hook 'css-mode-hook 'skewer-css-mode)
  (define-key skewer-mode-map (kbd "C-c C-p") #'skewer-eval-print-last-expression))

(provide 'setup-web)
