;;; init.el --- Init package -*- lexical-binding: t -*-
;;; Commentary:
;; My init

;;; Code:
(setq-default gc-cons-threshold 100000000)

(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("melpa" . "http://melpa.org/packages/")
        ("melpa-stable" . "http://stable.melpa.org/packages/")))

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile (require 'use-package))
(setq use-package-always-ensure t)

(add-to-list 'load-path "~/.emacs.d/my/")

;; Keep custom settings in separate file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

(require 'general)
(require 'setup-helm)
(require 'setup-org)
(require 'setup-lisp)
(require 'setup-python)
(require 'setup-php)
(require 'setup-web)

(setq-default gc-cons-threshold 800000)

(provide 'init)
;;; init.el ends here
