;;; init.el --- Init package -*- lexical-binding: t -*-
;;; Commentary:
;; My init

;;; Code:
(setq-default gc-cons-threshold most-positive-fixnum)

;; Normally file-name-handler-alist is set to
;; (("\\`/[^/]*\\'" . tramp-completion-file-name-handler)
;; ("\\`/[^/|:][^/|]*:" . tramp-file-name-handler)
;; ("\\`/:" . file-name-non-special))
;; Which means on every .el and .elc file loaded during start up, it has to runs those regexps against the filename.
(let ((file-name-handler-alist nil))

  ;; (package-initialize)

  (dolist (l (directory-files "~/.emacs.d/lib" nil "^[^\.]"))
    (add-to-list 'load-path (concat "~/.emacs.d/lib/" l)))

  (dolist (l (directory-files "~/.emacs.d/themes" nil "^[^\.]"))
    (add-to-list 'load-path (concat "~/.emacs.d/themes/" l))
    (add-to-list 'custom-theme-load-path (concat "~/.emacs.d/themes/" l)))

  (add-to-list 'load-path "~/.emacs.d/my/")

  (eval-when-compile (require 'use-package))

  ;; Keep custom settings in separate file
  (setq custom-file (expand-file-name "custom.el" user-emacs-directory))
  (load custom-file)

  (require 'general)
  (require 'setup-org)
  (require 'setup-lisp)
  (require 'setup-python)
  (require 'setup-web))

(setq-default gc-cons-threshold 800000)

(provide 'init)
;;; init.el ends here
