;;; setup-scheme-mode.el --- Setup scheme-mode -*- lexical-binding: t -*-

(require 'cmuscheme)
(require 'dash)
(require 'f)
(require 's)

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

(define-key scheme-mode-map (kbd "C-C C-d") #'chicken-doc)
(define-key scheme-mode-map (kbd "C-M-x") #'scheme-send-definition)
(define-key scheme-mode-map (kbd "C-c C-h") #'counsel-semantic-or-imenu)
(define-key scheme-mode-map (kbd "C-c C-k") #'scheme-send-buffer)
(define-key scheme-mode-map (kbd "C-c C-p") #'scheme-send-print-last-sexp)
(define-key scheme-mode-map (kbd "C-c C-r")#'scheme-send-region)
(define-key scheme-mode-map (kbd "C-c C-z") #'run-chicken)
(define-key scheme-mode-map (kbd "C-x e") #'scheme-send-last-sexp)

(activate-chickenv)

(provide 'setup-scheme-mode)
