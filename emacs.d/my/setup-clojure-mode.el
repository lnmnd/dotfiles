;;; setup-clojure-mode.el --- Setup clojure-mode -*- lexical-binding: t -*-

(require 'dumb-jump)

(defun cljfmt ()
  (when (eq major-mode 'clojure-mode)
    (shell-command-to-string (format "cljfmt %s" buffer-file-name))
    (revert-buffer :ignore-auto :noconfirm)))

(add-hook 'after-save-hook #'cljfmt)

(define-key clojure-mode-map (kbd "C-c C-h") #'counsel-semantic-or-imenu)
(define-key clojure-mode-map (kbd "C-c C-i") #'dumb-jump-go)
(define-key clojure-mode-map (kbd "C-c C-o") #'dumb-jump-back)

(defun clj-connect ()
  (interactive)
  (inf-clojure-connect "localhost" 6666))

(defun clj-add-lib ()
  (interactive)
  (let ((coordinates (read-string "Coordinates: ")))
    (when (get-buffer "*inf-clojure*")
      (send-string "*inf-clojure*"
		   (concat
		    "(in-ns 'user)"
		    "(require '[clojure.tools.deps.alpha.repl :refer [add-lib]])"
		    "(add-lib " coordinates ")")))))

(defun clj-start ()
  (interactive)
  (when (get-buffer "*inf-clojure*")
    (comint-simple-send "*inf-clojure*" "(ns user) (start)\n")))

(defun clj-stop ()
  (interactive)
  (when (get-buffer "*inf-clojure*")
    (comint-simple-send "*inf-clojure*" "(ns user) (stop)\n")))

(defun clj-restart ()
  (interactive)
  (when (get-buffer "*inf-clojure*")
    (comint-simple-send "*inf-clojure*" "(ns user) (stop) (start)\n")))

(defun clj-unmap ()
  (interactive)
  (when (get-buffer "*inf-clojure*")
    (comint-simple-send "*inf-clojure*" (concat "(ns-unmap *ns* '" (symbol-name (symbol-at-point)) ")"))))

(defun clj-run-tests ()
  (interactive)
  (when (get-buffer "*inf-clojure*")
    (comint-simple-send "*inf-clojure*" "(require '[clojure.test :refer [run-tests]]) (run-tests)")))

(defun clj-eval-print-last-sexp ()
  (interactive)
  (when (get-buffer "*inf-clojure*")
    (let* ((start (save-excursion (backward-sexp) (point)))
	   (end (point))
	   (sexp (buffer-substring-no-properties start end))
	   (string (concat "(require '[clojure.pprint :refer [pprint]]) (pprint " sexp ")")))
      (comint-simple-send "*inf-clojure*" string))))

(require 'inf-clojure)
(add-hook 'clojure-mode-hook #'inf-clojure-minor-mode)
(setup-company)
(define-key inf-clojure-minor-mode-map (kbd "C-c M-c") #'clj-connect)
(define-key inf-clojure-minor-mode-map (kbd "C-c C-k") #'inf-clojure-eval-buffer)
(define-key inf-clojure-minor-mode-map (kbd "C-c C-d") #'inf-clojure-show-var-documentation)
(define-key inf-clojure-minor-mode-map (kbd "C-c C-u") #'clj-unmap)
(define-key inf-clojure-minor-mode-map (kbd "C-c C-i") #'dumb-jump-go) ;; override inf-clojure-show-ns-vars
(define-key inf-clojure-minor-mode-map (kbd "C-c C-t") #'clj-run-tests)
(define-key inf-clojure-minor-mode-map (kbd "C-c C-p") #'clj-eval-print-last-sexp)

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
(defun cider-restart ()
  (interactive)
  (cider-interactive-eval
   "(in-ns 'user) (stop) (start)"))

(autoload 'cider-connect "cider" "" t)
(eval-after-load 'cider
  '(progn
     (define-key cider-mode-map (kbd "C-c C-d") #'cider-doc)
     (define-key cider-mode-map (kbd "C-c C-r") #'cider-eval-region)
     (define-key cider-mode-map (kbd "C-c C-t") #'cider-run-tests)))

(provide 'setup-clojure-mode)
