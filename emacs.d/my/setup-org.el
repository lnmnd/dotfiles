;;; setup-org.el --- Setup Org-mode -*- lexical-binding: t -*-

;; markdown export
(require 'ox-md)

(setq org-todo-keywords '((sequence "TODO" "STARTED" "|" "DONE" "CANCELED" "POSTPONED")))
(setq org-agenda-files '("~/org/"))
;; no postample (footer)
(setq org-html-postamble nil)
;; export to HTML5
(setq org-html-doctype "html5")
(setq org-capture-templates
      '(("t" "Todo" entry (file "~/org/tasks.org")
	 "* TODO %?\n%a" :empty-lines 1)))

(setq org-confirm-babel-evaluate nil)
(setq org-plantuml-jar-path
      (expand-file-name "~/local/opt/plantuml.jar"))
(org-babel-do-load-languages
 'org-babel-load-languages
 '(;; other Babel languages
   (plantuml . t)
   (restclient . t)))

(add-hook 'org-mode-hook #'enable-show-trailing-whitespace)
(add-hook 'org-mode-hook #'auto-fill-mode)

(provide 'setup-org)
