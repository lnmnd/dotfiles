;;; setup-org.el --- Setup Org-mode -*- lexical-binding: t -*-

(use-package
  org
  :init
  (setq org-todo-keywords
        '((sequence "TODO" "STARTED" "|" "DONE" "CANCELED" "POSTPONED")))
  (setq org-todo-keyword-faces
        (quote (("TODO" :foreground "yellow" :weight bold)
                ("STARTED" :foreground "blue" :weight bold)
                ("DONE" :foreground "green" :weight bold)
                ("CANCELED" :foreground "red" :weight bold)
                ("POSTPONED" :foreground "orange" :weight bold))))
  (setq org-agenda-files '("~/org/"))
  ;; no postample (footer)
  (setq org-html-postamble nil)
  ;; export to HTML5
  (setq org-html-doctype "html5")
  ;; markdown export
  (eval-after-load "org"
    '(require 'ox-md nil t))

  (setq org-confirm-babel-evaluate nil)
  (setq org-plantuml-jar-path
        (expand-file-name "~/local/opt/plantuml.jar"))
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '(;; other Babel languages
     (plantuml . t)))

  (add-hook 'org-mode-hook #'enable-show-trailing-whitespace))

(use-package
  org-pomodoro
  :config
  (setq org-pomodoro-length 25))

(provide 'setup-org)
