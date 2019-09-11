;;; setup-lisp.el --- Setup Lisp -*- lexical-binding: t -*-

(require 'dash)
(require 'f)
(require 'popup)
(require 's)

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
(setq chickenv-activated nil)

(defun setup-lisp--find-chicken-version-dir ()
  (locate-dominating-file (buffer-file-name) ".chicken-version"))

(defun setup-lisp--read-text (path)
  (f-read-text path 'utf-8))

(defun setup-lisp--set-chickenv (path)
  (setenv "CHICKEN_INSTALL_REPOSITORY" path)
  (setenv "CHICKEN_REPOSITORY_PATH" path))

(defun activate-chickenv ()
  (interactive)
  (when (not chickenv-activated)
    (let ((chicken-version-dir (setup-lisp--find-chicken-version-dir)))
      (when chicken-version-dir
	(->> chicken-version-dir
	     (f-expand ".chicken-version")
	     setup-lisp--read-text
	     s-trim
	     (concat "/home/user/.chickenv/")
	     setup-lisp--set-chickenv)
	(setq chickenv-activated t)))))

(defun run-chicken ()
  (interactive)
  (run-scheme "csi"))

(defun chicken-doc ()
  (interactive)
  (->> (current-word)
       (format "(require-library chicken-doc) ,doc %S\n\n")
       (process-send-string (scheme-proc)))
  (save-selected-window
    (select-window (display-buffer (get-buffer scheme-buffer) t))
    (goto-char (point-max))))

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

(add-hook 'scheme-mode-hook #'activate-chickenv)

(use-package
  cmuscheme
  :config
  (define-key scheme-mode-map (kbd "C-C C-d") #'chicken-doc)
  (define-key scheme-mode-map (kbd "C-M-x") #'scheme-send-definition)
  (define-key scheme-mode-map (kbd "C-c C-h") #'counsel-semantic-or-imenu)
  (define-key scheme-mode-map (kbd "C-c C-k") #'scheme-send-buffer)
  (define-key scheme-mode-map (kbd "C-c C-p") #'scheme-send-print-last-sexp)
  (define-key scheme-mode-map (kbd "C-c C-r")#'scheme-send-region)
  (define-key scheme-mode-map (kbd "C-c C-z") #'run-chicken)
  (define-key scheme-mode-map (kbd "C-x e") #'scheme-send-last-sexp))

(use-package
  clojure-mode
  :config
  (add-hook 'after-save-hook #'cljfmt)
  (define-key clojure-mode-map (kbd "C-c C-h") #'counsel-semantic-or-imenu)
  (define-key clojure-mode-map (kbd "C-c C-i") #'dumb-jump-go)
  (define-key clojure-mode-map (kbd "C-c C-o") #'dumb-jump-back))

(use-package
  cider
  :config
  (define-key cider-mode-map (kbd "C-c C-d") #'cider-doc)
  (define-key cider-mode-map (kbd "C-c C-r") #'cider-eval-region)
  (define-key cider-mode-map (kbd "C-c C-t") #'cider-run-tests)

  (defun cider-add-lib ()
    (interactive)
    (let ((coordinates (read-string "Coordinates: ")))
      (cider-interactive-eval
       (concat
	"(in-ns 'user)"
	"(require '[clojure.tools.deps.alpha.repl :refer [add-lib]])"
	"(add-lib " coordinates ")"))))
  (defun cider-rebl ()
    (interactive)
    (cider-interactive-eval
     "(in-ns 'user) (require '[cognitect.rebl]) (cognitect.rebl/ui)"))
  (defun cider-start ()
    (interactive)
    (cider-interactive-eval
     "(in-ns 'user) (start)"))
  (defun cider-stop ()
    (interactive)
    (cider-interactive-eval
     "(in-ns 'user) (stop)"))
  (defun cider-reset ()
    (interactive)
    (cider-interactive-eval
     "(in-ns 'user) (stop) (start)")))

(provide 'setup-lisp)
