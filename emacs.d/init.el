;;; init --- Init package
;;; Commentary:
;; My init

;;; Code:
(require 'package)
(add-to-list 'package-archives
	     '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
             '("melpa-stable" . "http://stable.melpa.org/packages/") t)
(add-to-list 'package-archives
             '("elpy" . "http://jorgenschaefer.github.io/packages/"))
(add-to-list 'load-path "~/.emacs/lib")
(package-initialize)

(load (expand-file-name "~/.emacs.d/lib/epresent.el"))

(defun load-my (file)
    (load (expand-file-name (concat "~/.emacs.d/my/" (symbol-name file)))))

(load-my 'general)
(load-my 'helm)
(load-my 'org)
(load-my 'lisp)
(load-my 'python)
(load-my 'web)
(load-my 'factor)

(provide 'init)
;;; init.el ends here
