;;; init.el --- Init package -*- lexical-binding: t -*-
;;; Commentary:
;; My init

;;; Code:
(setq gc-cons-threshold most-positive-fixnum)
(setq gc-cons-percentage 0.6)

;; Normally file-name-handler-alist is set to
;; (("\\`/[^/]*\\'" . tramp-completion-file-name-handler)
;; ("\\`/[^/|:][^/|]*:" . tramp-file-name-handler)
;; ("\\`/:" . file-name-non-special))
;; Which means on every .el and .elc file loaded during start up, it has to runs those regexps against the filename.
(setq file-name-handler-alist-orig file-name-handler-alist)
(setq file-name-handler-alist nil)

(add-hook 'emacs-startup-hook
	  (lambda ()
	    (setq gc-cons-threshold 16777216)
	    (setq gc-cons-percentage 0.1)
	    (setq file-name-handler-alist file-name-handler-alist-orig)))

(dolist (l (directory-files "~/.emacs.d/lib" nil "^[^\.]"))
  (add-to-list 'load-path (concat "~/.emacs.d/lib/" l)))

(dolist (l (directory-files "~/.emacs.d/themes" nil "^[^\.]"))
  (add-to-list 'load-path (concat "~/.emacs.d/themes/" l))
  (add-to-list 'custom-theme-load-path (concat "~/.emacs.d/themes/" l)))

(add-to-list 'load-path "~/.emacs.d/my/")

;; Keep custom settings in separate file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

(load "main")

(provide 'init)
;;; init.el ends here
