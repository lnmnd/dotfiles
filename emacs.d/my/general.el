;;; general.el --- General -*- lexical-binding: t -*-

;; no welcome screen
(setq inhibit-splash-screen t)

;; Remove the menu bar
(menu-bar-mode -1)
;;; Remove the tool bar
(tool-bar-mode -1)

(setq frame-title-format "%b - emacs")

(add-to-list 'default-frame-alist '(font . "Fira Code-12"))
(custom-set-variables
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(custom-enabled-themes (quote (tango))))

;; backups
(setq backup-directory-alist `(("." . "~/.emacs.d/backups")))
(setq backup-by-copying t)
(setq delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

;; Set the return key to globally act as a new-line-and-intent
(global-set-key "\C-m" 'newline-and-indent)

(define-key key-translation-map (kbd "<f9> t") (kbd "~"))
(define-key key-translation-map (kbd "<f9> l") (kbd "λ"))

(setq-default show-trailing-whitespace t)

;; no rebase-mode
(setq auto-mode-alist (delete '("git-rebase-todo" . rebase-mode)
                              auto-mode-alist))

(setq vc-follow-symlinks t)
(electric-pair-mode)

(setq tags-revert-without-query 1)

(use-package
  undo-tree
  :config
  (global-undo-tree-mode))

;; global company mode
(use-package
  company
  :config
  (add-hook 'after-init-hook 'global-company-mode)
  ;; auto-completion starts from one character
  (setq company-minimum-prefix-length 1)
  ;; keys
  (define-key company-active-map (kbd "M-n") nil)
  (define-key company-active-map (kbd "M-p") nil)
  (define-key company-active-map (kbd "C-n") #'company-select-next)
  (define-key company-active-map (kbd "C-p") #'company-select-previous))

(use-package
  projectile
  :init
  (setq projectile-enable-caching t)
  :config
  (projectile-global-mode))

(use-package
  magit
  :config
  (setq magit-last-seen-setup-instructions "1.4.0"))

(use-package
  flycheck
  :config
  (add-hook 'php-mode-hook 'flycheck-mode)
  (setq flycheck-phpmd-rulesets '("cleancode"
                                  "codesize"
                                  "controversial"
                                  "design"
                                  "unusedcode"))
  (setq flycheck-phpcs-standard "PSR2")
  (set-face-attribute 'flycheck-error nil :underline "Red1")
  (set-face-attribute 'flycheck-warning nil :underline "orange red")

  (add-hook 'c-mode-hook 'flycheck-mode))

(use-package
  isend-mode
  :config
  ;; C-RET to send the current line to the interpreter
  (setq isend-forward-line nil)
  (setq isend-skip-empty-lines nil)
  (setq isend-strip-empty-lines nil)
  (setq isend-delete-indentation t)
  (setq isend-end-with-empty-line t))

(use-package
  avy
  :config
  (setq avy-keys (nconc (number-sequence ?a ?z) (number-sequence ?A ?Z)))
  :bind
  ("C-c a j" . avy-goto-word-or-subword-1))

(use-package markdown-mode)
(use-package json-mode)
(use-package yaml-mode)
(use-package csv-mode)
(use-package recompile-on-save)
(use-package suggest)

(provide 'general)
