(setq lexical-binding t)

(setq scheme-program-name "csi")

(add-to-list 'auto-mode-alist '("\\.sxml\\'" . scheme-mode))
(add-to-list 'auto-mode-alist '("\\.scss\\'" . scheme-mode))
(add-to-list 'auto-mode-alist '("\\.spock\\'" . scheme-mode))

;; highlight matching brackets when your cursor is on one of the bracket
(show-paren-mode 1)

(setq
 hl-paren-colors '()
 hl-paren-background-colors (list "#4f4f4f" "#4f4f4f" "#4f4f4f" "#4f4f4f" "#4f4f4f" "#4f4f4f" "#4f4f4f"))

(setq my-scheme-hooks '(scheme-mode-hook emacs-lisp-mode-hook))
(dolist (x my-scheme-hooks)
  (add-hook x
	    (lambda ()
	      (highlight-parentheses-mode t))))


;; paredit
(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
(add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
(add-hook 'ielm-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
(add-hook 'scheme-mode-hook           #'enable-paredit-mode)

;; clojure
(add-hook 'clojure-mode-hook 'paredit-mode)
(defun cider-namespace-refresh ()
  (interactive)
  (cider-interactive-eval
   "(require 'clojure.tools.namespace.repl)
    (clojure.tools.namespace.repl/refresh)"))
