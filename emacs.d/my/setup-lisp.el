;;; setup-lisp.el --- Setup Lisp -*- lexical-binding: t -*-

(require 'popup)

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
	    (define-key emacs-lisp-mode-map "\C-c\C-h" #'counsel-semantic-or-imenu)
	    (define-key emacs-lisp-mode-map "\C-c\C-i" #'xref-find-definitions)
	    (define-key emacs-lisp-mode-map "\C-c\C-k" #'eval-buffer)
	    (define-key emacs-lisp-mode-map "\C-c\C-o" #'xref-pop-marker-stack)
	    (define-key emacs-lisp-mode-map "\C-c\C-p" #'eval-print-last-sexp)
	    (define-key emacs-lisp-mode-map "\C-c\C-r" #'eval-region)
	    (define-key emacs-lisp-mode-map "\C-x\C-e" #'my-eval-last-sexp)))

(setq scheme-program-name "csi")

(defun chicken-doc (&optional obtain-function)
  (interactive)
  (let ((func (funcall (or obtain-function #'current-word))))
    (when func
      (process-send-string (scheme-proc)
                           (format "(require-library chicken-doc) ,doc %S\n" func))
      (save-selected-window
        (select-window (display-buffer (get-buffer scheme-buffer) t))
        (goto-char (point-max))))))

(defun scheme-send-buffer ()
  (interactive)
  (save-excursion
    (scheme-send-region (point-min) (point-max))))

(defun scheme-send-print-last-sexp ()
  (interactive)
  (let* ((start (save-excursion (backward-sexp) (point)))
	 (end (point))
	 (string (concat
                  "(begin "
                  "(import (only (chicken pretty-print) pp))"
                  "(pp " (buffer-substring-no-properties start end) "))")))
    (comint-send-string (scheme-proc) string)
    (comint-send-string (scheme-proc) "\n")))

(use-package
  cmuscheme
  :config
  (define-key scheme-mode-map (kbd "C-C C-d") #'chicken-doc)
  (define-key scheme-mode-map (kbd "C-M-x") #'scheme-send-definition)
  (define-key scheme-mode-map (kbd "C-c C-k") #'scheme-send-buffer)
  (define-key scheme-mode-map (kbd "C-c C-p") #'scheme-send-print-last-sexp)
  (define-key scheme-mode-map (kbd "C-c C-r")#'scheme-send-region)
  (define-key scheme-mode-map (kbd "C-x e") #'scheme-send-last-sexp))

(use-package
  clojure-mode
  :config
  (add-hook 'after-save-hook #'cljfmt)
  (defun cider-add-lib ()
    (interactive)
    (let ((coordinates (read-string "Coordinates: ")))
      (cider-interactive-eval
       (concat
	"(in-ns 'user)"
	"(require '[clojure.tools.deps.alpha.repl :refer [add-lib]])"
	"(add-lib " coordinates ")"))))
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
     "(in-ns 'user) (halt)"))
  (defun cider-rebl ()
    (interactive)
    (cider-interactive-eval
     "(in-ns 'user) (require '[cognitect.rebl]) (cognitect.rebl/ui)"))
  (defun cider-run-tests ()
    (interactive)
    (cider-interactive-eval
     "(run-tests)")))

(use-package
  cider
  :config
  (define-key cider-mode-map (kbd "C-c C-d") #'cider-doc)
  (define-key cider-mode-map (kbd "C-c C-h") #'counsel-semantic-or-imenu)
  (define-key cider-mode-map (kbd "C-c C-i") #'cider-find-var)
  (define-key cider-mode-map (kbd "C-c C-o") #'cider-pop-back)
  (define-key cider-mode-map (kbd "C-c C-r") #'cider-eval-region)
  (define-key cider-mode-map (kbd "C-c C-t") #'cider-run-tests))

(provide 'setup-lisp)
