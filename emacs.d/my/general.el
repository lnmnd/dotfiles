;;; general.el --- General -*- lexical-binding: t -*-

;; no welcome screen
(setq inhibit-splash-screen t)

(setq initial-scratch-message "")

(setq frame-title-format "%b - emacs")

(load-theme 'bare t)

;; backups
(setq backup-directory-alist `(("." . "~/.emacs.d/backups")))
(setq backup-by-copying t)
(setq delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

(fset 'yes-or-no-p 'y-or-n-p)

;; Set the return key to globally act as a new-line-and-intent
(global-set-key "\C-m" 'newline-and-indent)

(define-key key-translation-map (kbd "<f9> t") (kbd "~"))
(define-key key-translation-map (kbd "<f9> l") (kbd "λ"))

;; no rebase-mode
(setq auto-mode-alist (delete '("git-rebase-todo" . rebase-mode)
                              auto-mode-alist))

(setq vc-follow-symlinks t)
(electric-pair-mode)

(setq tags-revert-without-query 1)

;; highlight matching brackets when your cursor is on one of the bracket
(show-paren-mode 1)

(setq browse-url-browser-function 'eww-browse-url)

(setq compilation-ask-about-save nil)

(setq dired-listing-switches "-alh")
(setq dired-dwim-target t)

(setq-default fill-column 80)

(global-hi-lock-mode 1)

(defun kill-all-buffers ()
  (interactive)
  (mapc 'kill-buffer (buffer-list)))

(defun enable-show-trailing-whitespace ()
  (setq show-trailing-whitespace t))

(setq mode-line-format2 nil)

(defun toggle-mode-line ()
  (interactive)
  (let ((tmp mode-line-format))
    (setq-local mode-line-format mode-line-format2)
    (setq-local mode-line-format2 tmp)))

(use-package diminish)

(use-package
  undo-tree
  :diminish undo-tree-mode
  :config
  (global-undo-tree-mode))

;; global company mode
(use-package
  company
  :diminish company-mode
  :config
  (add-hook 'after-init-hook 'global-company-mode)
  ;; auto-completion starts from one character
  (setq company-minimum-prefix-length 1)
  ;; keys
  (define-key company-active-map (kbd "M-n") nil)
  (define-key company-active-map (kbd "M-p") nil)
  (define-key company-active-map (kbd "C-n") #'company-select-next)
  (define-key company-active-map (kbd "C-p") #'company-select-previous))

(use-package ivy
  :diminish ivy-mode
  :config
  (setq ivy-height 20)
  (ivy-mode 1))

(use-package counsel
  :after (ivy)
  :diminish counsel-mode
  :config
  (counsel-mode 1))

(use-package swiper
  :bind
  ("C-c C-g" . swiper))

(use-package
  projectile
  :after (ivy)
  :diminish projectile-mode
  :init
  (setq projectile-enable-caching t)
  :config
  (setq projectile-completion-system 'ivy)
  (projectile-mode +1)
  (define-key projectile-mode-map (kbd "C-c p") #'projectile-command-map))

(use-package counsel-projectile
  :after (projectile ivy)
  :config
  (counsel-projectile-mode 1))

(use-package
  magit
  :load-path "lib/magit/lisp"
  :bind
  ("C-x g" . magit-status))

(use-package
  flycheck
  :config
  (setq flycheck-idle-change-delay 1)
  (setq flycheck-check-syntax-automatically '(save idle-change mode-enabled))
  (set-face-attribute 'flycheck-error nil :underline "Red1")
  (set-face-attribute 'flycheck-warning nil :underline "orange red"))

(use-package
  avy
  :config
  (setq avy-keys (nconc (number-sequence ?a ?z) (number-sequence ?A ?Z)))
  (set-face-attribute 'avy-lead-face nil :foreground "red")
  (set-face-attribute 'avy-lead-face nil :background "#fbf8ef")
  :bind
  ("C-c a j" . avy-goto-word-or-subword-1))

(use-package
  autorevert
  :diminish auto-revert-mode)

(use-package yasnippet
  :diminish yas-minor-mode
  :config (yas-global-mode))

(use-package dumb-jump
  :config
  (setq dump-jump-default-project "~/code")
  (setq dumb-jump-selector 'ivy)
  (setq dumb-jump-prefer-searcher 'ag))

(use-package markdown-mode
  :mode "\\.md\\'"
  :config
  (add-hook 'markdown-mode-hook #'auto-fill-mode))

(use-package
  cc-mode
  :config
  (add-hook 'c-mode-hook #'flycheck-mode))

(use-package
  nov
  :mode ("\\.epub\\'" . nov-mode))

(use-package
  pdf-tools
  :load-path "lib/pdf-tools/lisp"
  :mode "\\.pdf\\'"
  :config
  (require 'pdf-annot)
  (require 'pdf-history)
  (require 'pdf-occur)
  (require 'pdf-outline)
  (require 'pdf-sync)
  (require 'pdf-util)
  (require 'pdf-virtual)
  (pdf-tools-install))

(use-package
  org-pdftools
  :config
  (add-hook 'org-mode-hook (lambda () (require 'org-pdftools))))

(use-package
  org-recoll
  :config
  (setq org-recoll-file-search-automatically nil))

;; Set elfeed-feeds in custom.el:
;; (setq elfeed-feeds
;;       '(("http://nullprogram.com/feed/" emacs)))
(use-package
  elfeed)

(provide 'general)
