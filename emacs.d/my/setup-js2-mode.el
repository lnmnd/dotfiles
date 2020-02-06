;;; setup-js2-mode.el --- Setup js2-mode -*- lexical-binding: t -*-

(require 'flycheck)
(require 'skewer-mode)
(require 'yasnippet)

(defun my-js-format-code ()
  (shell-command-to-string (format "eslint --fix %s" buffer-file-name))
  (revert-buffer :ignore-auto :noconfirm))

(defun js-load-sc ()
  (interactive)
  (find-file (expand-file-name "~/.emacs.d/my/js-scope-capture.txt"))
  (skewer-load-buffer)
  (kill-buffer))

(defun js-start ()
  (interactive)
  (skewer-eval "start(c);" (lambda (_) (message "Started"))))

(defun js-stop ()
  (interactive)
  (skewer-eval "stop(c);" (lambda (_) (message "Stopped"))))

(defun js-restart ()
  (interactive)
  (skewer-eval "stop(c); start(c);" (lambda (_) (message "Restarted"))))

(defun js-eval ()
  (interactive)
  (skewer-eval (read-string "Eval:") #'skewer-post-minibuffer))

(define-key js2-mode-map (kbd "C-c C-h") #'counsel-semantic-or-imenu)
(define-key js2-mode-map (kbd "C-c C-i") 'js2-jump-to-definition)
(define-key js2-mode-map (kbd "C-c C-o") 'xref-pop-marker-stack)
(define-key js2-mode-map (kbd "C-c C-t") #'recompile)
(define-key skewer-mode-map (kbd "C-c C-p") #'skewer-eval-print-last-expression)
(defun setup-js2-mode-hook ()
  (setq indent-tabs-mode nil)
  (setq js2-basic-offset 2)
  (enable-show-trailing-whitespace)
  (flycheck-mode)
  (skewer-mode)
  (yas-reload-all)
  (yas-minor-mode)
  (setup-company)
  (add-hook 'after-save-hook #'my-js-format-code nil t))
(add-hook 'js2-mode-hook #'setup-js2-mode-hook)

(provide 'setup-js2-mode)
