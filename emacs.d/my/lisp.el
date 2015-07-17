(setq lexical-binding t)

(setq scheme-program-name "csi")

(add-to-list 'auto-mode-alist '("\\.sxml\\'" . scheme-mode))
(add-to-list 'auto-mode-alist '("\\.scss\\'" . scheme-mode))
(add-to-list 'auto-mode-alist '("\\.spock\\'" . scheme-mode))

;; highlight matching brackets when your cursor is on one of the bracket
(show-paren-mode 1)

(use-package
  highlight-parentheses
  :ensure t
  :init
  (setq
   hl-paren-colors '()
   hl-paren-background-colors (list "#4f4f4f" "#4f4f4f" "#4f4f4f" "#4f4f4f" "#4f4f4f" "#4f4f4f" "#4f4f4f"))
  (dolist (x '(scheme-mode-hook emacs-lisp-mode-hook))
    (add-hook x (lambda ()
		  (highlight-parentheses-mode t)))))

(use-package
  clojure-mode
  :ensure t
  :config
  (defun cider-namespace-refresh ()
    (interactive)
    (cider-interactive-eval
     "(require 'clojure.tools.namespace.repl)
    (clojure.tools.namespace.repl/refresh)")))

(use-package
  cider
  :ensure t
  :pin melpa-stable)

(use-package
  geiser
  :ensure t)

(use-package
  paredit
  :ensure t
  :init
  (dolist (x '(emacs-lisp-mode-hook
	       eval-expression-minibuffer-setup-hook
	       ielm-mode-hook
	       lisp-mode-hook
	       lisp-interaction-mode-hook
	       scheme-mode-hook
	       clojure-mode-hook))
    (add-hook x #'enable-paredit-mode)))
