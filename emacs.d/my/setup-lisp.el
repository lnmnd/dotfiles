;;; setup-lisp.el --- Setup Lisp -*- lexical-binding: t -*-

(use-package popup)

(defun cljfmt ()
  (when (eq major-mode 'clojure-mode)
    (shell-command-to-string (format "cljfmt %s" buffer-file-name))
    (revert-buffer :ignore-auto :noconfirm)))

(use-package
  highlight-parentheses
  :diminish highlight-parentheses-mode
  :init
  (dolist (x '(emacs-lisp-mode-hook
	       lisp-mode-hook
	       scheme-mode-hook
	       clojure-mode-hook))
    (add-hook x (lambda ()
                  (highlight-parentheses-mode t)))))

(use-package
  paredit
  :diminish paredit-mode
  :init
  (dolist (x '(emacs-lisp-mode-hook
               eval-expression-minibuffer-setup-hook
               ielm-mode-hook
               lisp-mode-hook
               lisp-interaction-mode-hook
               scheme-mode-hook
               clojure-mode-hook))
    (add-hook x #'enable-paredit-mode)
    (add-hook x #'enable-show-trailing-whitespace)))

(defun my-eval-last-sexp ()
  (interactive)
  (let ((result (pp-to-string (eval (pp-last-sexp) lexical-binding))))
    (popup-tip result :point (point-at-bol))))

(add-hook 'emacs-lisp-mode-hook
	  (lambda ()
	    (define-key emacs-lisp-mode-map "\C-c\C-o" #'xref-pop-marker-stack)
	    (define-key emacs-lisp-mode-map "\C-c\C-b" #'eval-buffer)
	    (define-key emacs-lisp-mode-map "\C-c\C-i" #'xref-find-definitions)
	    (define-key emacs-lisp-mode-map "\C-c\C-p" #'eval-print-last-sexp)
	    (define-key emacs-lisp-mode-map "\C-c\C-r" #'eval-region)
	    (define-key emacs-lisp-mode-map "\C-x\C-e" #'my-eval-last-sexp)))

(setq scheme-program-name "csi")

(use-package
  clojure-mode
  :config
  (add-hook 'after-save-hook #'cljfmt)
  (defun cider-prep ()
    (interactive)
    (cider-interactive-eval
     "(in-ns 'user) (prep)"))
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
  :config
  (define-key cider-mode-map (kbd "C-c C-d") #'cider-doc)
  (define-key cider-mode-map (kbd "C-c C-i") #'cider-find-var)
  (define-key cider-mode-map (kbd "C-c C-o") #'cider-pop-back)
  (define-key cider-mode-map (kbd "C-c C-r") #'cider-eval-region))

(use-package spiral
  :config
  (define-key spiral-mode-map (kbd "C-M-x") #'spiral-eval-top-level-form)
  (define-key spiral-mode-map (kbd "C-c C-k") #'spiral-eval-buffer)
  (define-key spiral-mode-map (kbd "C-c C-e") #'spiral-eval-last-sexp)
  (define-key spiral-mode-map (kbd "C-x C-e") #'spiral-eval-last-sexp)
  (define-key spiral-mode-map (kbd "C-c C-p") #'spiral-inspect-last-eval))

(provide 'setup-lisp)
