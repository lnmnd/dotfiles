;;; init.el --- Init package -*- lexical-binding: t -*-
;;; Commentary:
;; My init

;;; Code:
(setq-default gc-cons-threshold 100000000)

(require 'package)
(add-to-list 'package-archives
	     '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives
             '("melpa-stable" . "http://stable.melpa.org/packages/"))
(add-to-list 'package-archives
             '("elpy" . "http://jorgenschaefer.github.io/packages/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(eval-when-compile (require 'use-package))
(setq use-package-always-ensure t)

(add-to-list 'load-path "~/.emacs.d/my/")

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
