;;; setup-lisp.el --- Setup Lisp -*- lexical-binding: t -*-

(setq scheme-program-name "csi")

(add-to-list 'auto-mode-alist '("\\.sxml\\'" . scheme-mode))
(add-to-list 'auto-mode-alist '("\\.spock\\'" . scheme-mode))

;; highlight matching brackets when your cursor is on one of the bracket
(show-paren-mode 1)

(use-package
  highlight-parentheses
  :init
  (setq
   hl-paren-colors '()
   hl-paren-background-colors (list "#4f4f4f" "#4f4f4f" "#4f4f4f" "#4f4f4f" "#4f4f4f" "#4f4f4f" "#4f4f4f"))
  (dolist (x '(scheme-mode-hook emacs-lisp-mode-hook))
    (add-hook x (lambda ()
		  (highlight-parentheses-mode t)))))

(use-package
  paredit
  :init
  (dolist (x '(emacs-lisp-mode-hook
	       eval-expression-minibuffer-setup-hook
	       ielm-mode-hook
	       lisp-mode-hook
	       lisp-interaction-mode-hook
	       scheme-mode-hook
	       clojure-mode-hook
	       hy-mode-hook))
    (add-hook x #'enable-paredit-mode)))

(use-package
  clj-refactor
  :diminish clj-refactor-mode)

(use-package
  clojure-mode
  :config
  (defun cider-namespace-refresh ()
    (interactive)
    (cider-interactive-eval
     "(require 'clojure.tools.namespace.repl)
    (clojure.tools.namespace.repl/refresh)"))
  (defun cider-init ()
    (interactive)
    (cider-interactive-eval
     "(in-ns 'user) (init)"))
  (defun cider-stop ()
    (interactive)
    (cider-interactive-eval
     "(in-ns 'user) (stop)"))
  (defun cider-start ()
    (interactive)
    (cider-interactive-eval
     "(in-ns 'user) (start)"))
  (defun cider-go ()
    (interactive)
    (cider-interactive-eval
     "(in-ns 'user) (go)"))
  (defun cider-reset ()
    (interactive)
    (cider-interactive-eval
     "(in-ns 'user) (reset)"))
  (defun cider-halt ()
    (interactive)
    (cider-interactive-eval
     "(in-ns 'user) (halt)"))  )

(use-package
  cider
  :pin melpa-stable
  :init (add-hook 'cider-mode-hook #'clj-refactor-mode))

(use-package cljr-helm)

(provide 'setup-lisp)
